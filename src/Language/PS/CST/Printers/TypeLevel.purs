module Language.PS.CST.Printers.TypeLevel where

import Prelude

import Language.PS.CST.Printers.Utils
import Language.PS.CST.Types.Declaration (Constraint(..), DataCtor(..), DataHead(..), Kind(..), Row, Type(..), TypeVarBinding(..))
import Language.PS.CST.Types.QualifiedName (QualifiedName(..))
import Language.PS.CST.Types.Leafs (ClassFundep(..), Fixity(..), Ident, Label, OpName, ProperName)
import Language.PS.CST.ReservedNames (appendUnderscoreIfReserved, quoteIfReserved)

import Data.Array (snoc) as Array
import Data.Foldable (null)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Text.Pretty
import Text.Pretty.Symbols.String
import Data.Container.Class

printFundep :: ClassFundep -> Doc String
printFundep (FundepDetermines lefts rights) = (vsep $ map (text <<< appendUnderscoreIfReserved <<< unwrap) lefts) <+> text "->" <+> (vsep $ map (text <<< appendUnderscoreIfReserved <<< unwrap) rights)

printFixity :: Fixity -> Doc String
printFixity Infix  = text "infix"
printFixity Infixl = text "infixl"
printFixity Infixr = text "infixr"

printDataCtor :: DataCtor -> Doc String
printDataCtor (DataCtor dataCtor) =
  let
    doWrap :: Type -> Boolean
    doWrap (TypeApp _ _) = true
    doWrap (TypeForall _ _) = true
    doWrap (TypeArr _ _) = true
    doWrap (TypeOp _ _ _) = true
    doWrap (TypeConstrained _ _) = true
    doWrap _ = false

    context = PrintType_Multiline

    printType' :: Type -> Doc String
    printType' type_ = maybeWrapInParentheses (doWrap type_) $ printType context $ type_

    name = (text <<< appendUnderscoreIfReserved <<< unwrap) dataCtor.dataCtorName

    fields = dataCtor.dataCtorFields <#> printType'
  in
    vcat $ [name] <> fields

printDataHead :: Doc String -> DataHead -> Doc String
printDataHead reservedWord (DataHead dataHead) =
  let
    head = reservedWord <+> (text <<< appendUnderscoreIfReserved <<< unwrap) dataHead.dataHdName

    vars = map printTypeVarBinding dataHead.dataHdVars
  in
    if null vars then head else head <+> vsep vars

printTypeVarBinding :: TypeVarBinding -> Doc String
printTypeVarBinding (TypeVarName ident) = (text <<< appendUnderscoreIfReserved <<< unwrap) ident
printTypeVarBinding (TypeVarKinded ident kind_) = parens $ (text <<< appendUnderscoreIfReserved <<< unwrap) ident <+> text "::" <+> printKind kind_

printKind :: Kind -> Doc String
printKind (KindName qualifiedKindName) = printQualifiedName_AnyProperNameType qualifiedKindName
printKind (KindArr kindLeft_ kindRight_) =
  let
    isComplex :: Kind -> Boolean
    isComplex (KindArr _ _) = true
    isComplex _ = false

    printedLeft = printKind kindLeft_

    printedLeft' = if isComplex kindLeft_ then parens printedLeft else printedLeft
  in
    printedLeft' <+> text "->" <+> printKind kindRight_
printKind (KindRow kind_) = text "#" <+> printKind kind_

printQualifiedName_Ident :: QualifiedName Ident -> Doc String
printQualifiedName_Ident (QualifiedName qualifiedName) = case qualifiedName.qualModule of
  Nothing -> (text <<< appendUnderscoreIfReserved <<< unwrap) qualifiedName.qualName
  (Just moduleName) -> printModuleName moduleName <> text "." <> (text <<< appendUnderscoreIfReserved <<< unwrap) qualifiedName.qualName

printQualifiedName_AnyProperNameType :: ∀ proxy. QualifiedName (ProperName proxy) -> Doc String
printQualifiedName_AnyProperNameType (QualifiedName qualifiedName) = case qualifiedName.qualModule of
  Nothing -> (text <<< appendUnderscoreIfReserved <<< unwrap) qualifiedName.qualName
  (Just moduleName) -> printModuleName moduleName <> text "." <> (text <<< appendUnderscoreIfReserved <<< unwrap) qualifiedName.qualName

printQualifiedName_AnyOpNameType :: ∀ proxy. QualifiedName (OpName proxy) -> Doc String
printQualifiedName_AnyOpNameType (QualifiedName qualifiedName) = case qualifiedName.qualModule of
  Nothing -> (text <<< appendUnderscoreIfReserved <<< unwrap) qualifiedName.qualName
  (Just moduleName) -> printModuleName moduleName <> text "." <> parens ((text <<< appendUnderscoreIfReserved <<< unwrap) qualifiedName.qualName)

-- Prefer multiline when first enter the rendering function, prefer one line when inside of row extensions (i.e. `MyExt + MyOtherExt` in `( foo :: Bar | MyExt + MyOtherExt )`)
data PrintType_Style
  = PrintType_Multiline
  | PrintType_OneLine

printType :: PrintType_Style -> Type -> Doc String
printType printType_Style (TypeVar ident) = (text <<< appendUnderscoreIfReserved <<< unwrap) ident
printType printType_Style (TypeConstructor qualifiedTypeName) = printQualifiedName_AnyProperNameType qualifiedTypeName
printType printType_Style TypeWildcard = text "_"
printType printType_Style (TypeHole ident) = text "?" <> (text <<< appendUnderscoreIfReserved <<< unwrap) ident
printType printType_Style (TypeString string) = dquotes $ text string
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
         printedLeft :: Doc String
         printedLeft = printType PrintType_OneLine leftType

         printedRight :: Doc String
         printedRight = printType PrintType_OneLine rightType

         printed :: Doc String
         printed = printedLeft <+> maybeWrapInParentheses doWrapRight printedRight
     Identity printed
printType printType_Style (TypeForall typeVarBindings type_) =
  let
    newContext = printType_Style
  in
    text "forall" <+> vsep (map printTypeVarBinding typeVarBindings) <+> text "." <+> printType newContext type_
printType printType_Style (TypeArr leftType rightType) =
  let
    newContext = printType_Style
  in
    printType newContext leftType <+> text "->" <+> printType newContext rightType
printType printType_Style (TypeKinded type_ kind_) =
  let
    newContext = printType_Style
  in
    parens $ printType newContext type_ <+> text "::" <+> printKind kind_
printType printType_Style (TypeOp leftType qualifiedOpName rightType) =
  let
    newContext = printType_Style
  in
    printType newContext leftType <+> printQualifiedName_AnyOpNameType qualifiedOpName <+> printType newContext rightType
printType printType_Style (TypeConstrained constraint type_) =
  let
    newContext = PrintType_OneLine
  in
    printConstraint constraint <+> text "=>" <+> printType newContext type_

printConstraint :: Constraint -> Doc String
printConstraint (Constraint { className, args }) =
  let
    context = PrintType_OneLine
  in
    if null args
      then printQualifiedName_AnyProperNameType className
      else printQualifiedName_AnyProperNameType className <+> (vsep $ map (printType context) args)

printRowLikeType :: PrintType_Style -> Doc String -> Doc String -> Row -> Doc String
printRowLikeType _ leftWrapper rightWrapper row@({ rowLabels: [], rowTail: Nothing }) = leftWrapper <> rightWrapper
printRowLikeType _ leftWrapper rightWrapper row@({ rowLabels: [], rowTail: Just rowTail }) =
  let
    context = PrintType_OneLine
  in
    leftWrapper <+> text "|" <+> printType context rowTail <+> rightWrapper
printRowLikeType PrintType_OneLine leftWrapper rightWrapper row@({ rowLabels, rowTail }) =
  let
    context = PrintType_OneLine

    printedTail = rowTail <#> printType context <#> (text "|" <+> _)

    printedRowLabels =
      rowLabels
        <#> printRowLabel context
        # concatWith (surround (text ", "))
        # maybe identity (\tail rowLine -> rowLine <+> tail) printedTail
        # (\x -> leftWrapper <+> x <+> rightWrapper)
  in
    printedRowLabels
printRowLikeType PrintType_Multiline leftWrapper rightWrapper row =
  let
    context = PrintType_Multiline

    printedTail = row.rowTail <#> printType context <#> (text "|" <+> _)

    printedRowLabels =
      row.rowLabels
        <#> printRowLabel context
        # mapWithIndex (\i box -> text "," <+> box)
        # maybe identity (flip Array.snoc) printedTail
        # flip Array.snoc rightWrapper
        # vcat
  in
    printedRowLabels

printRowLabel :: PrintType_Style -> { label :: Label, type_ :: Type } -> Doc String
printRowLabel printType_Style { label, type_ } = (text <<< quoteIfReserved <<< unwrap) label <+> text "::" <+> printType printType_Style type_
