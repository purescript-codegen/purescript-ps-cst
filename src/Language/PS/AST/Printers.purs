module Language.PS.AST.Printers where

import Language.PS.AST.Printers.PrintImports
import Language.PS.AST.Printers.PrintModuleModuleNameAndExports
import Language.PS.AST.Printers.Utils
import Language.PS.AST.Types
import Prelude
import Text.PrettyPrint.Boxes

import Data.Array (cons, fromFoldable, null, snoc) as Array
import Data.Char.Unicode (isUpper)
import Data.Either (Either(..), fromRight)
import Data.Foldable (class Foldable, foldMap, intercalate, length, null)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..))
import Data.List (fromFoldable, intercalate) as List
import Data.Map (toUnfoldable) as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Set (member) as Set
import Data.String (joinWith)
import Data.String.CodeUnits (uncons) as SCU
import Data.String.Regex (Regex, regex)
import Data.String.Regex (test) as Regex
import Data.String.Regex.Flags (noFlags) as Regex.Flags
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicate)
import Matryoshka (Algebra, cata)
import Partial.Unsafe (unsafePartial)

printModule :: Module -> Box
printModule (Module { moduleName, imports, exports, declarations }) = lines $
  [ printModuleModuleNameAndExports moduleName exports
  , printImports imports
  , printDeclarations declarations
  , emptyRow
  ]

printDeclarations :: Array Declaration -> Box
printDeclarations [] = nullBox
printDeclarations declarations =
  emptyRow
  // (vsep 1 left $ map printDeclaration declarations)

printDeclaration :: Declaration -> Box
printDeclaration (DeclData dataHead []) = printDataHead dataHead
printDeclaration (DeclData dataHead arrayDataCtor) =
  let
    -- printedNamesColumn = vcat left $ map printDataCtor
    -- separatorsColumn = vcat left $ [text "="] <> replicate (length arrayDataCtor - 1) (text "|")
    -- printedCtors = twoSpaceIdentation <<>> separatorsColumn <<+>> printedNamesColumn
    printedCtors =
      arrayDataCtor
      <#> printDataCtor
      # mapWithIndex (\i box -> ifelse (i == 0) (text "=") (text "|") <<+>> box)
      <#> (twoSpaceIdentation <<>> _)
      # vcat left
  in printDataHead dataHead // printedCtors
printDeclaration (DeclType dataHead type_) = nullBox

printDataCtor :: DataCtor -> Box
printDataCtor (DataCtor dataCtor) =
  let
    doWrap :: Type -> Boolean
    doWrap (TypeVar _) = false
    doWrap (TypeConstructor _) = false
    doWrap TypeWildcard = false
    doWrap (TypeHole _) = false
    doWrap (TypeString _) = false
    doWrap (TypeRow _) = false
    doWrap (TypeRecord _) = false
    doWrap (TypeApp _ _) = true
    doWrap (TypeForall _ _) = true
    doWrap (TypeArr _ _) = true
    doWrap (TypeKinded _ _) = false
    doWrap (TypeOp _ _ _) = true
    doWrap (TypeConstrained _ _) = true

    printType' :: Type -> Box
    printType' type_ = if doWrap type_ then wrapInParentheses <<< printType PreferMultiline $ type_ else printType PreferMultiline type_

    name = textFromNewtype dataCtor.dataCtorName
    fields = dataCtor.dataCtorFields <#> printType'
    printedFields = vcat left fields
  in name <<+>> printedFields

printDataHead :: DataHead -> Box
printDataHead (DataHead dataHead) =
  let
    head = text "data" <<+>> textFromNewtype dataHead.dataHdName
    vars = map printTypeVarBinding dataHead.dataHdVars
   in if null vars then head else head <<+>> punctuateH left (text " ") vars

printTypeVarBinding :: TypeVarBinding -> Box
printTypeVarBinding (TypeVarName ident) = textFromNewtype ident
printTypeVarBinding (TypeVarKinded ident kind_) = wrapInParentheses $ textFromNewtype ident <<+>> text "::" <<+>> printKind kind_

printKind :: Kind -> Box
printKind (KindName qualifiedKindName) = printQualifiedName_AnyProperNameType qualifiedKindName
printKind (KindArr kindLeft_ kindRight_) =
  let
    isComplex :: Kind -> Boolean
    isComplex (KindName _) = false
    isComplex (KindArr _ _) = true
    isComplex (KindRow _) = false
    isComplex (KindParens _) = false

    printedLeft = printKind kindLeft_
    printedLeft' = if isComplex kindLeft_ then wrapInParentheses printedLeft else printedLeft
   in
    printedLeft' <<+>> text "->" <<+>> printKind kindRight_
printKind (KindRow kind_) = text "#" <<+>> printKind kind_
printKind (KindParens kind_) = wrapInParentheses $ printKind kind_

printQualifiedName_AnyProperNameType :: ∀ proxy . QualifiedName (ProperName proxy) -> Box
printQualifiedName_AnyProperNameType (QualifiedName qualifiedName) =
  case qualifiedName.qualModule of
       Nothing -> textFromNewtype qualifiedName.qualName
       (Just moduleName) -> printModuleName moduleName <<>> text "." <<>> textFromNewtype qualifiedName.qualName

printQualifiedName_AnyOpNameType :: ∀ proxy . QualifiedName (OpName proxy) -> Box
printQualifiedName_AnyOpNameType (QualifiedName qualifiedName) =
  case qualifiedName.qualModule of
       Nothing -> textFromNewtype qualifiedName.qualName
       (Just moduleName) -> printModuleName moduleName <<>> text "." <<>> wrapInParentheses (textFromNewtype qualifiedName.qualName)

data PrintTypeStyle = PreferMultiline | PreferOneLine
-- data IsInsideOfApp = IsInsideOfApp_Yes | IsInsideOfApp_No -- Am I inside of TypeApp that didn't yet break? used to determine whether to

printType :: PrintTypeStyle -> Type -> Box
printType printTypeStyle (TypeVar ident)                             = textFromNewtype ident
printType printTypeStyle (TypeConstructor qualifiedTypeName)         = printQualifiedName_AnyProperNameType qualifiedTypeName
printType printTypeStyle TypeWildcard                                = text "_"
printType printTypeStyle (TypeHole ident)                            = text "?" <<>> textFromNewtype ident
printType printTypeStyle (TypeString string)                         = text "\"" <<>> text string <<>> text "\""
printType printTypeStyle (TypeRow row)                               = printRowLikeType printTypeStyle (text "(") (text ")") row
printType printTypeStyle (TypeRecord row)                            = printRowLikeType printTypeStyle (text "{") (text "}") row
printType printTypeStyle (TypeApp leftType rightType)                =
  let
    -- doWrapLeft :: Type -> Boolean
    -- doWrapLeft (TypeVar _) = false
    -- doWrapLeft (TypeConstructor _) = false
    -- doWrapLeft TypeWildcard = false
    -- doWrapLeft (TypeHole _) = false
    -- doWrapLeft (TypeString _) = false
    -- doWrapLeft (TypeRow _) = false
    -- doWrapLeft (TypeRecord _) = false
    -- doWrapLeft (TypeApp _ _) = true
    -- doWrapLeft (TypeForall _ _) = true
    -- doWrapLeft (TypeArr _ _) = true
    -- doWrapLeft (TypeKinded _ _) = false
    -- doWrapLeft (TypeOp _ _ _) = true
    -- doWrapLeft (TypeConstrained _ _) = true

    printedLeft = printType PreferOneLine leftType
    -- printedLeft' = if doWrapLeft leftType then wrapInParentheses printedLeft else printedLeft
    printed = printedLeft <<+>> printType PreferOneLine rightType
   in printed
printType printTypeStyle (TypeForall typeVarBindings type_)          = text "forall" <<+>> punctuateH left (text " ") (map printTypeVarBinding typeVarBindings) <<+>> text "." <<+>> printType printTypeStyle type_
printType printTypeStyle (TypeArr leftType rightType)                = printType printTypeStyle leftType <<+>> text "->" <<+>> printType printTypeStyle rightType
printType printTypeStyle (TypeKinded type_ kind_)                    = text "(" <<>> printType printTypeStyle type_ <<+>> text "::" <<+>> printKind kind_ <<>> text ")"
printType printTypeStyle (TypeOp leftType qualifiedOpName rightType) = printType printTypeStyle leftType <<+>> printQualifiedName_AnyOpNameType qualifiedOpName <<+>> printType printTypeStyle rightType
printType printTypeStyle (TypeConstrained constraint type_)          = printConstraint constraint <<+>> text "=>" <<+>> printType PreferOneLine type_

printConstraint :: Constraint -> Box
printConstraint (Constraint { className, args }) = printQualifiedName_AnyProperNameType className <<+>> (punctuateH left (text " ") $ map (printType PreferOneLine) args)
printConstraint (ConstraintParens constraint) = wrapInParentheses $ printConstraint constraint

printRowLikeType :: PrintTypeStyle -> Box -> Box -> Row -> Box
printRowLikeType printTypeStyle leftWrapper rightWrapper row@(Row { rowLabels: [], rowTail: Nothing }) = leftWrapper <<>> rightWrapper
printRowLikeType printTypeStyle leftWrapper rightWrapper row@(Row { rowLabels: [], rowTail: Just rowTail }) = leftWrapper <<+>> text "|" <<+>> printType PreferOneLine rowTail <<+>> rightWrapper
printRowLikeType PreferOneLine leftWrapper rightWrapper row@(Row { rowLabels, rowTail }) =
  let
    printedTail = rowTail <#> printType PreferOneLine <#> (text "|" <<+>> _)

    printedRowLabels =
      rowLabels
      <#> printRowLabel PreferOneLine
      # punctuateH left (text ",")
      # maybe identity (\tail rowLine -> rowLine <<+>> tail) printedTail
      # (\x -> leftWrapper <<+>> x <<+>> rightWrapper)
   in
    printedRowLabels
printRowLikeType PreferMultiline leftWrapper rightWrapper row@(Row { rowLabels, rowTail }) =
  let
    printedTail = rowTail <#> printType PreferMultiline <#> (text "|" <<+>> _)

    printedRowLabels =
      rowLabels
      <#> printRowLabel PreferMultiline
      # mapWithIndex (\i box -> ifelse (i == 0) leftWrapper (text ",") <<+>> box)
      # maybe identity (flip Array.snoc) printedTail
      # flip Array.snoc rightWrapper
      -- <#> (twoSpaceIdentation <<>> _)
      # vcat left
   in
    printedRowLabels

printRowLabel :: PrintTypeStyle -> { label :: Label, type_ :: Type } -> Box
printRowLabel printTypeStyle { label, type_ } = textFromNewtype label <<+>> text "::" <<+>> printType printTypeStyle type_

ifelse :: forall a. Boolean -> a -> a -> a
ifelse p a b = if (p) then a else b
