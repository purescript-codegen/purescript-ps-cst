module Language.PS.CST.Printers.TypeLevel where

import Dodo
import Prelude

import Data.Array as Array
import Data.Foldable (null)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Debug.Trace
import Language.PS.CST.Printers.Utils
import Language.PS.CST.ReservedNames (appendUnderscoreIfReserved, quoteIfReserved)
import Language.PS.CST.Types.Declaration (Constraint(..), DataCtor(..), DataHead(..), Kind(..), Row, Type(..), TypeVarBinding(..))
import Language.PS.CST.Types.Leafs (ClassFundep(..), Fixity(..), Ident, Label, OpName, ProperName)
import Language.PS.CST.Types.QualifiedName (QualifiedName(..))

printFundep :: ClassFundep -> Doc Void
printFundep (FundepDetermines lefts rights) = (paragraph $ map (text <<< appendUnderscoreIfReserved <<< unwrap) lefts) <+> text "->" <+> (paragraph $ map (text <<< appendUnderscoreIfReserved <<< unwrap) rights)

printFixity :: Fixity -> Doc Void
printFixity Infix  = text "infix"
printFixity Infixl = text "infixl"
printFixity Infixr = text "infixr"

printDataCtor :: DataCtor -> Doc Void
printDataCtor (DataCtor dataCtor) =
  let
    doWrap :: Type -> Boolean
    doWrap (TypeApp _ _) = true
    doWrap (TypeForall _ _) = true
    doWrap (TypeArr _ _) = true
    doWrap (TypeOp _ _ _) = true
    doWrap (TypeConstrained _ _) = true
    doWrap _ = false

    printType' :: Type -> Doc Void
    printType' type_ = maybeWrapInParentheses (doWrap type_) $ printType type_

    name = (text <<< appendUnderscoreIfReserved <<< unwrap) dataCtor.dataCtorName

    fields = dataCtor.dataCtorFields <#> printType'
  in
    flexGroup $ foldWithSeparator spaceBreak $ [name, foldWithSeparator softBreak $ fields]

printDataHead :: Doc Void -> DataHead -> Doc Void
printDataHead reservedWord (DataHead dataHead) =
  let
    head = reservedWord <+> (text <<< appendUnderscoreIfReserved <<< unwrap) dataHead.dataHdName

    vars = map printTypeVarBinding dataHead.dataHdVars
  in
    if null vars then head else head <+> paragraph vars

printTypeVarBinding :: TypeVarBinding -> Doc Void
printTypeVarBinding (TypeVarName ident) = (text <<< appendUnderscoreIfReserved <<< unwrap) ident
printTypeVarBinding (TypeVarKinded ident kind_) = parens $ (text <<< appendUnderscoreIfReserved <<< unwrap) ident <+> text "::" <+> printKind kind_

printKind :: Kind -> Doc Void
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

printQualifiedName_Ident :: QualifiedName Ident -> Doc Void
printQualifiedName_Ident (QualifiedName qualifiedName) = case qualifiedName.qualModule of
  Nothing -> (text <<< appendUnderscoreIfReserved <<< unwrap) qualifiedName.qualName
  (Just moduleName) -> printModuleName moduleName <> text "." <> (text <<< appendUnderscoreIfReserved <<< unwrap) qualifiedName.qualName

printQualifiedName_AnyProperNameType :: ∀ proxy. QualifiedName (ProperName proxy) -> Doc Void
printQualifiedName_AnyProperNameType (QualifiedName qualifiedName) = case qualifiedName.qualModule of
  Nothing -> (text <<< appendUnderscoreIfReserved <<< unwrap) qualifiedName.qualName
  (Just moduleName) -> printModuleName moduleName <> text "." <> (text <<< appendUnderscoreIfReserved <<< unwrap) qualifiedName.qualName

printQualifiedName_AnyOpNameType :: ∀ proxy. QualifiedName (OpName proxy) -> Doc Void
printQualifiedName_AnyOpNameType (QualifiedName qualifiedName) = case qualifiedName.qualModule of
  Nothing -> (text <<< appendUnderscoreIfReserved <<< unwrap) qualifiedName.qualName
  (Just moduleName) -> printModuleName moduleName <> text "." <> parens ((text <<< appendUnderscoreIfReserved <<< unwrap) qualifiedName.qualName)

printType :: Type -> Doc Void
printType = \type_ -> case type_ of
                           (TypeApp _ _) -> flexGroup $ printTypeImplementation type_
                           (TypeForall _ _) -> flexGroup $ printTypeImplementation type_
                           (TypeConstrained _ _) -> flexGroup $ printTypeImplementation type_
                           (TypeArr _ _) -> flexGroup $ printTypeImplementation type_
                           _ -> printTypeImplementation type_
  where
    printTypeImplementation (TypeVar ident) = (text <<< appendUnderscoreIfReserved <<< unwrap) ident
    printTypeImplementation (TypeConstructor qualifiedTypeName) = printQualifiedName_AnyProperNameType qualifiedTypeName
    printTypeImplementation TypeWildcard = text "_"
    printTypeImplementation (TypeHole ident) = text "?" <> (text <<< appendUnderscoreIfReserved <<< unwrap) ident
    printTypeImplementation (TypeString string) = dquotes $ text string
    printTypeImplementation (TypeRow row) = printRowLikeType (text "(") (text ")") row
    printTypeImplementation (TypeRecord row) = printRowLikeType (text "{") (text "}") row
    printTypeImplementation (TypeApp leftType rightType) =
      let
        doWrapRight =
          case rightType of
            (TypeApp _ _) -> true -- always wrap right side application
            (TypeForall _ _) -> true
            (TypeArr _ _) -> true
            (TypeOp _ _ _) -> true
            (TypeConstrained _ _) -> true
            _ -> false

        printedLeft :: Doc Void
        printedLeft = printTypeImplementation leftType

        printedRight :: Doc Void
        printedRight = printTypeImplementation rightType
      in alignCurrentColumn $ foldWithSeparator spaceBreak [ printedLeft, maybeWrapInParentheses doWrapRight printedRight ]
    printTypeImplementation (TypeForall typeVarBindings type_) = text "forall" <+> foldWithSeparator spaceBreak (map printTypeVarBinding typeVarBindings) <+> text "." <+> printTypeImplementation type_
    printTypeImplementation (TypeArr leftType rightType) = printTypeImplementation leftType <+> text "->" <+> printTypeImplementation rightType
    printTypeImplementation (TypeKinded type_ kind_) = parens $ printTypeImplementation type_ <+> text "::" <+> printKind kind_
    printTypeImplementation (TypeOp leftType qualifiedOpName rightType) = printTypeImplementation leftType <+> printQualifiedName_AnyOpNameType qualifiedOpName <+> printTypeImplementation rightType
    printTypeImplementation (TypeConstrained constraint type_) = printConstraint constraint <+> text "=>" <+> printTypeImplementation type_

printConstraint :: Constraint -> Doc Void
printConstraint (Constraint { className, args }) =
  if null args
    then printQualifiedName_AnyProperNameType className
    else printQualifiedName_AnyProperNameType className <+> (alignCurrentColumn $ flexGroup $ foldWithSeparator spaceBreak $ map printType args)

printRowLikeType :: Doc Void -> Doc Void -> Row -> Doc Void
printRowLikeType leftWrapper rightWrapper row =
  let
      rowTail :: Doc Void
      rowTail =
        maybe
        mempty
        (\(rowTail' :: Type) -> text "|" <+> printType rowTail')
        row.rowTail

   in case row.rowLabels of
           [] ->
             if isEmpty rowTail
               then leftWrapper <> rightWrapper
               else flexGroup $ foldWithSeparator spaceBreak [leftWrapper, rowTail, rightWrapper]
           _ ->
             let
                 rowLabelDocs :: Array (Doc Void)
                 rowLabelDocs = row.rowLabels <#> printRowLabel
              in alignCurrentColumn $ flexGroup $ foldWithSeparator spaceBreak
                [ foldWithSeparator softBreak $ Array.zipWith (<>) ([leftWrapper <> space] <> Array.replicate (Array.length rowLabelDocs - 1) (text ", ")) rowLabelDocs
                , rowTail
                , rightWrapper
                ]

printRowLabel :: { label :: Label, type_ :: Type } -> Doc Void
printRowLabel { label, type_ } = (text <<< quoteIfReserved <<< unwrap) label <+> text "::" <+> printType type_
