module Language.PS.CST.Printers.TypeLevel where

import Prelude

import Language.PS.CST.Printers.Utils (emptyColumn, ifelse, maybeWrapInParentheses, printModuleName, wrapInDoubleQuotes, wrapInParentheses)
import Language.PS.CST.Types.Shared (ClassFundep(..), Constraint(..), DataCtor(..), DataHead(..), Fixity(..), Ident, Kind(..), Label, OpName, ProperName, Row(..), Type(..), TypeVarBinding(..))
import Language.PS.CST.Types.QualifiedName (QualifiedName(..))
import Language.PS.CST.ReservedNames (appendUnderscoreIfReserved, quoteIfReserved)

import Data.Array (snoc) as Array
import Data.Foldable (null)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Text.PrettyPrint.Boxes (Box, left, punctuateH, text, vcat, (<<+>>), (<<>>))

printFundep :: ClassFundep -> Box
printFundep (FundepDetermines lefts rights) = (punctuateH left emptyColumn $ map (text <<< appendUnderscoreIfReserved <<< unwrap) lefts) <<+>> text "->" <<+>> (punctuateH left emptyColumn $ map (text <<< appendUnderscoreIfReserved <<< unwrap) rights)

printFixity :: Fixity -> Box
printFixity Infix  = text "infix"
printFixity Infixl = text "infixl"
printFixity Infixr = text "infixr"

printDataCtor :: DataCtor QualifiedName -> Box
printDataCtor (DataCtor dataCtor) =
  let
    doWrap :: Type QualifiedName -> Boolean
    doWrap (TypeApp _ _) = true
    doWrap (TypeForall _ _) = true
    doWrap (TypeArr _ _) = true
    doWrap (TypeOp _ _ _) = true
    doWrap (TypeConstrained _ _) = true
    doWrap _ = false

    context = PrintType_Multiline

    printType' :: Type QualifiedName -> Box
    printType' type_ = maybeWrapInParentheses (doWrap type_) $ printType context $ type_

    name = (text <<< appendUnderscoreIfReserved <<< unwrap) dataCtor.dataCtorName

    fields = dataCtor.dataCtorFields <#> printType'

    printedFields = vcat left fields
  in
    name <<+>> printedFields

printDataHead :: Box -> DataHead QualifiedName -> Box
printDataHead reservedWord (DataHead dataHead) =
  let
    head = reservedWord <<+>> (text <<< appendUnderscoreIfReserved <<< unwrap) dataHead.dataHdName

    vars = map printTypeVarBinding dataHead.dataHdVars
  in
    if null vars then head else head <<+>> punctuateH left (emptyColumn) vars

printTypeVarBinding :: TypeVarBinding QualifiedName -> Box
printTypeVarBinding (TypeVarName ident) = (text <<< appendUnderscoreIfReserved <<< unwrap) ident
printTypeVarBinding (TypeVarKinded ident kind_) = wrapInParentheses $ (text <<< appendUnderscoreIfReserved <<< unwrap) ident <<+>> text "::" <<+>> printKind kind_

printKind :: Kind QualifiedName -> Box
printKind (KindName qualifiedKindName) = printQualifiedName_AnyProperNameType qualifiedKindName
printKind (KindArr kindLeft_ kindRight_) =
  let
    isComplex :: Kind QualifiedName -> Boolean
    isComplex (KindArr _ _) = true
    isComplex _ = false

    printedLeft = printKind kindLeft_

    printedLeft' = if isComplex kindLeft_ then wrapInParentheses printedLeft else printedLeft
  in
    printedLeft' <<+>> text "->" <<+>> printKind kindRight_
printKind (KindRow kind_) = text "#" <<+>> printKind kind_

printQualifiedName_Ident :: QualifiedName Ident -> Box
printQualifiedName_Ident (QualifiedName qualifiedName) = case qualifiedName.qualModule of
  Nothing -> (text <<< appendUnderscoreIfReserved <<< unwrap) qualifiedName.qualName
  (Just moduleName) -> printModuleName moduleName <<>> text "." <<>> (text <<< appendUnderscoreIfReserved <<< unwrap) qualifiedName.qualName

printQualifiedName_AnyProperNameType :: ∀ proxy. QualifiedName (ProperName proxy) -> Box
printQualifiedName_AnyProperNameType (QualifiedName qualifiedName) = case qualifiedName.qualModule of
  Nothing -> (text <<< appendUnderscoreIfReserved <<< unwrap) qualifiedName.qualName
  (Just moduleName) -> printModuleName moduleName <<>> text "." <<>> (text <<< appendUnderscoreIfReserved <<< unwrap) qualifiedName.qualName

printQualifiedName_AnyOpNameType :: ∀ proxy. QualifiedName (OpName proxy) -> Box
printQualifiedName_AnyOpNameType (QualifiedName qualifiedName) = case qualifiedName.qualModule of
  Nothing -> (text <<< appendUnderscoreIfReserved <<< unwrap) qualifiedName.qualName
  (Just moduleName) -> printModuleName moduleName <<>> text "." <<>> wrapInParentheses ((text <<< appendUnderscoreIfReserved <<< unwrap) qualifiedName.qualName)

-- Prefer multiline when first enter the rendering function, prefer one line when inside of row extensions (i.e. `MyExt + MyOtherExt` in `( foo :: Bar | MyExt + MyOtherExt )`)
data PrintType_Style
  = PrintType_Multiline
  | PrintType_OneLine

printType :: PrintType_Style -> Type QualifiedName -> Box
printType printType_Style (TypeVar ident) = (text <<< appendUnderscoreIfReserved <<< unwrap) ident
printType printType_Style (TypeConstructor qualifiedTypeName) = printQualifiedName_AnyProperNameType qualifiedTypeName
printType printType_Style TypeWildcard = text "_"
printType printType_Style (TypeHole ident) = text "?" <<>> (text <<< appendUnderscoreIfReserved <<< unwrap) ident
printType printType_Style (TypeString string) = wrapInDoubleQuotes $ text string
printType printType_Style (TypeRow row) = printRowLikeType printType_Style (text "(") (text ")") row
printType printType_Style (TypeRecord row) = printRowLikeType printType_Style (text "{") (text "}") row
printType printType_Style (TypeApp leftType rightType) =
  let
    doWrapRight =
      case rightType of
        (TypeApp _ _) -> true -- always wrap right side application
        (TypeForall _ _) -> true
        (TypeArr _ _) -> true
        (TypeOp _ _ _) -> true
        (TypeConstrained _ _) -> true
        _ -> false
  in unwrap do
     -- traceM "--------------------------------"
     -- traceM "printType_Style.printType_IsInsideOfApp"
     -- traceM (printType_Style.printType_IsInsideOfApp)
     -- traceM "doWrapRight"
     -- traceM (doWrapRight)
     -- traceM "leftType"
     -- traceM leftType
     -- traceM "rightType"
     -- traceM rightType
     let
         printedLeft :: Box
         printedLeft = printType PrintType_OneLine leftType

         printedRight :: Box
         printedRight = printType PrintType_OneLine rightType

         printed :: Box
         printed = printedLeft <<+>> maybeWrapInParentheses doWrapRight printedRight
     Identity printed
printType printType_Style (TypeForall typeVarBindings type_) =
  let
    newContext = printType_Style
  in
    text "forall" <<+>> punctuateH left (emptyColumn) (map printTypeVarBinding typeVarBindings) <<+>> text "." <<+>> printType newContext type_
printType printType_Style (TypeArr leftType rightType) =
  let
    newContext = printType_Style
  in
    printType newContext leftType <<+>> text "->" <<+>> printType newContext rightType
printType printType_Style (TypeKinded type_ kind_) =
  let
    newContext = printType_Style
  in
    wrapInParentheses $ printType newContext type_ <<+>> text "::" <<+>> printKind kind_
printType printType_Style (TypeOp leftType qualifiedOpName rightType) =
  let
    newContext = printType_Style
  in
    printType newContext leftType <<+>> printQualifiedName_AnyOpNameType qualifiedOpName <<+>> printType newContext rightType
printType printType_Style (TypeConstrained constraint type_) =
  let
    newContext = PrintType_OneLine
  in
    printConstraint constraint <<+>> text "=>" <<+>> printType newContext type_

printConstraint :: Constraint QualifiedName -> Box
printConstraint (Constraint { className, args }) =
  let
    context = PrintType_OneLine
  in
    if null args
      then printQualifiedName_AnyProperNameType className
      else printQualifiedName_AnyProperNameType className <<+>> (punctuateH left (emptyColumn) $ map (printType context) args)

printRowLikeType :: PrintType_Style -> Box -> Box -> Row QualifiedName -> Box
printRowLikeType _ leftWrapper rightWrapper row@(Row { rowLabels: [], rowTail: Nothing }) = leftWrapper <<>> rightWrapper
printRowLikeType _ leftWrapper rightWrapper row@(Row { rowLabels: [], rowTail: Just rowTail }) =
  let
    context = PrintType_OneLine
  in
    leftWrapper <<+>> text "|" <<+>> printType context rowTail <<+>> rightWrapper
printRowLikeType PrintType_OneLine leftWrapper rightWrapper row@(Row { rowLabels, rowTail }) =
  let
    context = PrintType_OneLine

    printedTail = rowTail <#> printType context <#> (text "|" <<+>> _)

    printedRowLabels =
      rowLabels
        <#> printRowLabel context
        # punctuateH left (text ", ")
        # maybe identity (\tail rowLine -> rowLine <<+>> tail) printedTail
        # (\x -> leftWrapper <<+>> x <<+>> rightWrapper)
  in
    printedRowLabels
printRowLikeType PrintType_Multiline leftWrapper rightWrapper row@(Row { rowLabels, rowTail }) =
  let
    context = PrintType_Multiline

    printedTail = rowTail <#> printType context <#> (text "|" <<+>> _)

    printedRowLabels =
      rowLabels
        <#> printRowLabel context
        # mapWithIndex (\i box -> ifelse (i == 0) leftWrapper (text ",") <<+>> box)
        # maybe identity (flip Array.snoc) printedTail
        # flip Array.snoc rightWrapper
        # vcat left
  in
    printedRowLabels

printRowLabel :: PrintType_Style -> { label :: Label, type_ :: Type QualifiedName } -> Box
printRowLabel printType_Style { label, type_ } = (text <<< quoteIfReserved <<< unwrap) label <<+>> text "::" <<+>> printType printType_Style type_
