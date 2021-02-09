module Language.PS.CST.Printers.TypeLevel where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Dodo (Doc, alignCurrentColumn, bothNotEmpty, break, encloseEmptyAlt, flexAlt, flexGroup, foldWithSeparator, indent, paragraph, softBreak, space, spaceBreak, text, (<+>))
import Dodo.Common (leadingComma)
import Language.PS.CST.Printers.Utils (dquotesIf, labelNeedsQuotes, parens, printLabelledGroup, printModuleName, softSpace, unwrapText, (<%%>))
import Language.PS.CST.ReservedNames (appendUnderscoreIfReserved)
import Language.PS.CST.Types.Declaration (PSConstraint(..), Kind(..), PSRow, PSType(..), TypeVarBinding(..))
import Language.PS.CST.Types.Leafs (ClassFundep(..), Fixity(..), Ident, Label(..), OpName, ProperName)
import Language.PS.CST.Types.QualifiedName (QualifiedName(..))

printFundep :: ClassFundep -> Doc Void
printFundep (FundepDetermines lefts rights) = (paragraph $ map (text <<< appendUnderscoreIfReserved <<< unwrap) lefts) <+> text "->" <+> (paragraph $ unwrapText <$> rights)

printFixity :: Fixity -> Doc Void
printFixity Infix  = text "infix"
printFixity Infixl = text "infixl"
printFixity Infixr = text "infixr"

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

printType :: PSType -> Doc Void
printType = printType' false

printType' :: Boolean -> PSType -> Doc Void
printType' _ (TypeVar a) =  unwrapText a
printType' _ TypeWildcard = text "_"
printType' _ (TypeConstructor c) = printQualifiedName_AnyProperNameType c
printType' _ (TypeHole a) = text "?" <> unwrapText a
printType' _ (TypeString a) = text "\"" <> text a <> text "\""

printType' _ (TypeForall bs t) =
  ( text "forall" <+> (flexGroup (alignCurrentColumn (paragraph (printTypeVarBinding <$> bs))
                            )
                 )
  ) <> softBreak <> softSpace <> text ". " <> printType' false t
printType' _ (TypeKinded t k) =
  printLabelledGroup (printType' false t) (printKind k)

printType' _ (TypeRow r) =
  printRow "(" ")" r
printType' _ (TypeRecord r) =
  printRow "{" "}" r

printType' isWrapped (TypeApp a b) =
  foldWithSeparator sep apps

  where
    apps = collectApp a (NonEmptyArray.singleton (pp b))

    collectApp (TypeApp a' b') acc =
      collectApp a'
      (NonEmptyArray.cons (pp b') acc)
    collectApp t acc =
      NonEmptyArray.cons (pp t) acc

    pp t =
      let isWrapped' = needsParen t
      in parenIf isWrapped' (printType' isWrapped' t)

    needsParen (TypeKinded _ _) = true
    needsParen (TypeConstrained _ _) = true
    needsParen (TypeArr _ _) = true
    needsParen (TypeOp _ _ _) = true
    needsParen (TypeForall _ _) = true
    needsParen (TypeApp _ _) = true
    needsParen _ = false

    sep = flexAlt
      space
      ( break
        <> text "  "
        <> guard (not isWrapped) space
      )

printType' _ (TypeArr a b) =
  foldWithSeparator sep arrs

  where
    arrs = collectArr b (NonEmptyArray.singleton (pp a))

    collectArr (TypeArr a' b') acc =
      collectArr b'
      (NonEmptyArray.snoc acc (pp a'))
    collectArr t acc =
      NonEmptyArray.snoc acc (pp t)

    pp t =
      let isWrapped = needsParen t
      in parenIf isWrapped (printType' isWrapped t)

    needsParen (TypeKinded _ _) = true
    needsParen (TypeConstrained _ _) = true
    needsParen (TypeArr _ _) = true
    needsParen _ = false

    sep = spaceBreak <> text "-> "

printType' _ (TypeOp a op b) =
  flexGroup $ foldWithSeparator sep arrs

  where
    arrs = collectArr b (NonEmptyArray.singleton (pp a))

    collectArr (TypeOp a' op' b') acc | op == op' =
      collectArr b'
      (NonEmptyArray.snoc acc (pp a'))
    collectArr t acc =
      NonEmptyArray.snoc acc (pp t)

    pp t =
      let isWrapped = needsParen t
      in parenIf isWrapped (printType' isWrapped t)

    needsParen (TypeKinded _ _) = true
    needsParen (TypeConstrained _ _) = true
    needsParen (TypeArr _ _) = true
    needsParen (TypeOp _ op' _) = op' /= op
    needsParen _ = false

    opDoc = printQualifiedName_AnyOpNameType op
    sep = spaceBreak <> indent (opDoc <> space)

printType' _ (TypeConstrained c t) =
  printConstraint c <> spaceBreak <> text "=> " <> printType' false t

printRow :: String -> String -> PSRow -> Doc Void
printRow open close { rowLabels, rowTail } =
  alignCurrentColumn $
  encloseEmptyAlt (text open <> space) (spaceBreak <> text close) (text (open <> close)) $
  (foldWithSeparator leadingComma (printE <$> rowLabels))
  <%%> (fold $ printTl <$> rowTail)

  where
    printE { label: label@(Label name), type_ } =
      printLabelledGroup
      (dquotesIf (labelNeedsQuotes label) (text name))
      (printType' false type_)

    printTl t = text "| " <> printType' false t

printConstraint :: PSConstraint -> Doc Void
printConstraint (PSConstraint { className, args }) =
  foldWithSeparator sep apps

  where
    apps = Array.cons
           (printQualifiedName_AnyProperNameType className)
           (pp <$> args)

    pp t = parenIf (needsParen t) (printType' true t)

    needsParen (TypeKinded _ _) = true
    needsParen (TypeConstrained _ _) = true
    needsParen (TypeArr _ _) = true
    needsParen (TypeForall _ _) = true
    needsParen (TypeApp _ _) = true
    needsParen _ = false

    sep = spaceBreak

printConstraintList :: NonEmptyArray PSConstraint -> Doc Void
printConstraintList cs =
  parenIf (NonEmptyArray.length cs > 1)
  $ foldWithSeparator leadingComma (flexGroup <<< printConstraint <$> cs)

parenIf :: Boolean -> Doc Void -> Doc Void
parenIf true = paren
parenIf false = wrapEmpty

wrapEmpty :: Doc Void -> Doc Void
wrapEmpty = wrap mempty mempty

paren :: Doc Void -> Doc Void
paren = wrap "(" ")"

curlyBraces :: Doc Void -> Doc Void
curlyBraces = wrap "{" "}"

wrap :: String -> String -> Doc Void -> Doc Void
wrap open close d =
  flexGroup $ alignCurrentColumn $
  appendSoftBreak (appendSoftSpace (text open) d) (text close)
  where
    appendSoftSpace =
      bothNotEmpty \a b -> a <> (softSpace <> b)

    appendSoftBreak =
      bothNotEmpty \a b -> a <> (softBreak <> b)
