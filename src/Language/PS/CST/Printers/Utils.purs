module Language.PS.CST.Printers.Utils where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Foldable (class Foldable)
import Data.List (List(..), (:))
import Data.List (fromFoldable) as List
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Unsafe as Regex
import Data.String.Regex.Flags as RegexFlags
import Dodo (Doc, bothNotEmpty, break, enclose, encloseEmptyAlt, flexAlt, flexGroup, foldWithSeparator, indent, softBreak, space, spaceBreak, text, (<+>))
import Language.PS.CST.ReservedNames (appendUnderscoreIfReserved, isReservedName)
import Language.PS.CST.Types.Declaration (Declaration(..), Expr(..), InstanceBinding(..), LetBinding(..))
import Language.PS.CST.Types.Leafs (Label(..), ModuleName(..), ProperName, ProperNameType_ConstructorName)

-- | >>> dquotes "·"
-- "·"
dquotes :: forall a. Doc a -> Doc a
dquotes = enclose dquote dquote

dquotesIf :: forall a. Boolean -> Doc a -> Doc a
dquotesIf true = dquotes
dquotesIf false = identity

dquote :: forall a. Doc a
dquote = text "\""

-- | >>> parens "·"
-- (·)
parens :: forall a. Doc a -> Doc a
parens = enclose lparen rparen

-- | >>> lparen
-- (
lparen :: forall a. Doc a
lparen = text "("

-- | >>> rparen
-- )
rparen :: forall a. Doc a
rparen = text ")"

dot :: forall a. Doc a
dot = text "."

pursParensWithoutGroup :: forall a. Doc a -> Doc a
pursParensWithoutGroup = encloseEmptyAlt open close (text "()")
  where
  open = flexAlt (text "(") (text "( ")
  close = flexAlt (text ")") (break <> text ")")

printModuleName :: ModuleName -> Doc Void
printModuleName (ModuleName nonEmptyArray) = foldWithSeparator dot $ map (unwrap >>> text) nonEmptyArray

printConstructors :: Array (ProperName ProperNameType_ConstructorName) -> Doc Void
printConstructors = foldWithSeparator (text ", ") <<< map (text <<< unwrap)

foldWithPrev :: ∀ a b . (b -> Maybe a -> a -> b) -> b -> List a -> b
foldWithPrev _   default' Nil   = default'
foldWithPrev fun default' list = foo default' Nothing list
    where foo acc _    Nil     = acc
          foo acc prev (x : xs) = foo (fun acc prev x) (Just x) xs

maybeWrapInParentheses :: Boolean -> Doc Void -> Doc Void
maybeWrapInParentheses b = if b then parens else identity

printAndConditionallyAddNewlinesBetween :: ∀ a f . Foldable f => (a -> a -> Boolean) -> (a -> Doc Void) -> f a -> Doc Void
printAndConditionallyAddNewlinesBetween shouldBeNoNewlines print =
  let
    foldDeclaration :: Doc Void -> Maybe a -> a -> Doc Void
    foldDeclaration accum Nothing current = print current
    foldDeclaration accum (Just prev) current = if shouldBeNoNewlines prev current
                                                  then accum <> softBreak <> print current
                                                  else accum <> softBreak <> break <> (print current)
   in
    foldWithPrev foldDeclaration mempty <<< List.fromFoldable

shouldBeNoNewlineBetweenDeclarations :: Declaration -> Declaration -> Boolean
shouldBeNoNewlineBetweenDeclarations (DeclSignature { ident }) (DeclValue { valueBindingFields: { name } }) = ident == name
shouldBeNoNewlineBetweenDeclarations (DeclValue { valueBindingFields: { name } }) (DeclValue { valueBindingFields: { name: nameNext } }) = name == nameNext
shouldBeNoNewlineBetweenDeclarations _ _ = false

shouldBeNoNewlineBetweenLetBindings :: LetBinding -> LetBinding -> Boolean
shouldBeNoNewlineBetweenLetBindings (LetBindingSignature { ident }) (LetBindingName { name }) = ident == name
shouldBeNoNewlineBetweenLetBindings (LetBindingName { name }) (LetBindingName { name: nameNext }) = name == nameNext
shouldBeNoNewlineBetweenLetBindings _ _ = false

shouldBeNoNewlineBetweenInstanceBindings :: InstanceBinding -> InstanceBinding -> Boolean
shouldBeNoNewlineBetweenInstanceBindings (InstanceBindingSignature { ident }) (InstanceBindingName { name }) = ident == name
shouldBeNoNewlineBetweenInstanceBindings (InstanceBindingName { name }) (InstanceBindingName { name: nameNext }) = name == nameNext
shouldBeNoNewlineBetweenInstanceBindings _ _ = false

exprShouldBeOnNextLine :: Expr -> Boolean
exprShouldBeOnNextLine (ExprLet _) = true
exprShouldBeOnNextLine (ExprCase _) = true
exprShouldBeOnNextLine (ExprIf _) = true
exprShouldBeOnNextLine _ = false

labelNeedsQuotes :: Label -> Boolean
labelNeedsQuotes (Label name) =
  isReservedName name || not (Regex.test unquotedLabelRegex name)

unquotedLabelRegex :: Regex
unquotedLabelRegex =
  Regex.unsafeRegex "^[a-z][A-Za-z0-9_]*$" RegexFlags.noFlags

unwrapText :: forall a. Newtype a String => a -> Doc Void
unwrapText = text <<< appendUnderscoreIfReserved <<< unwrap

softSpace :: forall a. Doc a
softSpace =
  flexAlt mempty space

printSpaceSeparated :: NonEmptyArray (Doc Void) -> Doc Void
printSpaceSeparated apps =
  foldWithSeparator sep apps

  where
    sep = flexAlt space (break <> text "   ")

printLabelled :: Doc Void -> Doc Void -> Doc Void
printLabelled lbl ann =
  lbl <> spaceBreak <> indent (text "::" <+> ann)

printLabelledGroup :: Doc Void -> Doc Void -> Doc Void
printLabelledGroup lbl ann =
  flexGroup $ printLabelled lbl ann

appendSpaceBreakNoGroup :: forall a. Doc a -> Doc a -> Doc a
appendSpaceBreakNoGroup =
  bothNotEmpty \a b -> a <> spaceBreak <> b

infixr 2 appendSpaceBreakNoGroup as <%%>
