module Language.PS.CST.Printers.TypeLevel where

import Data.Container.Class
import Language.PS.CST.Printers.Utils
import Prelude
import Text.Pretty hiding (space)
import Text.Pretty.Symbols.String

import Data.Array as Array
import Data.Foldable (null)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Language.PS.CST.ReservedNames (appendUnderscoreIfReserved, quoteIfReserved)
import Language.PS.CST.Types.Declaration (Constraint(..), DataCtor(..), DataHead(..), Kind(..), Row, Type(..), TypeVarBinding(..))
import Language.PS.CST.Types.Leafs (ClassFundep(..), Fixity(..), Ident, Label, OpName, ProperName)
import Language.PS.CST.Types.QualifiedName (QualifiedName(..))
import Text.Pretty (concatWith)

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

    printType' :: Type -> Doc String
    printType' type_ = maybeWrapInParentheses (doWrap type_) $ printType type_

    name = (text <<< appendUnderscoreIfReserved <<< unwrap) dataCtor.dataCtorName

    fields = dataCtor.dataCtorFields <#> printType'
  in
    group $ concatWith (surroundOmittingEmpty line) $ [name, concatWith (surroundOmittingEmpty line') $ fields]

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

printType :: Type -> Doc String
printType = \type_ -> case type_ of
                           (TypeApp _ _) -> group $ printTypeImplementation type_
                           (TypeForall _ _) -> group $ printTypeImplementation type_
                           (TypeConstrained _ _) -> group $ printTypeImplementation type_
                           (TypeArr _ _) -> group $ printTypeImplementation type_
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

        printedLeft :: Doc String
        printedLeft = printTypeImplementation leftType

        printedRight :: Doc String
        printedRight = printTypeImplementation rightType
      in align $ concatWith (surround line) $ [ printedLeft, maybeWrapInParentheses doWrapRight printedRight ]
    printTypeImplementation (TypeForall typeVarBindings type_) = text "forall" <+> concatWithNonEmpty (surroundOmittingEmpty line) (map printTypeVarBinding typeVarBindings) <+> text "." <+> printTypeImplementation type_
    printTypeImplementation (TypeArr leftType rightType) = printTypeImplementation leftType <+> text "->" <+> printTypeImplementation rightType
    printTypeImplementation (TypeKinded type_ kind_) = parens $ printTypeImplementation type_ <+> text "::" <+> printKind kind_
    printTypeImplementation (TypeOp leftType qualifiedOpName rightType) = printTypeImplementation leftType <+> printQualifiedName_AnyOpNameType qualifiedOpName <+> printTypeImplementation rightType
    printTypeImplementation (TypeConstrained constraint type_) = printConstraint constraint <+> text "=>" <+> printTypeImplementation type_

printConstraint :: Constraint -> Doc String
printConstraint (Constraint { className, args }) =
  if null args
    then printQualifiedName_AnyProperNameType className
    else printQualifiedName_AnyProperNameType className <+> (align $ group $ concatWith (surround line) $ map printType args)

printRowLikeType :: Doc String -> Doc String -> Row -> Doc String
printRowLikeType leftWrapper rightWrapper row =
  let
      rowTail :: Doc String
      rowTail =
        maybe
        emptyDoc
        (\(rowTail' :: Type) -> text "|" <+> printType rowTail')
        row.rowTail

   in case row.rowLabels of
           [] ->
             case rowTail of
                  Empty -> leftWrapper <> rightWrapper
                  _     -> group $ concatWith (surroundOmittingEmpty line) [leftWrapper, rowTail, rightWrapper]
           _ ->
             let
                 rowLabelDocs :: Array (Doc String)
                 rowLabelDocs = row.rowLabels <#> printRowLabel
              in align $ group $ concatWith (surroundOmittingEmpty line)
                [ concatWith (surroundOmittingEmpty line') $ (Array.zipWith (<>) ([leftWrapper <> space] <> Array.replicate (Array.length rowLabelDocs - 1) (text ", ")) (map align rowLabelDocs))
                , rowTail
                , rightWrapper
                ]

printRowLabel :: { label :: Label, type_ :: Type } -> Doc String
printRowLabel { label, type_ } = (text <<< quoteIfReserved <<< unwrap) label <+> text "::" <+> printType type_
