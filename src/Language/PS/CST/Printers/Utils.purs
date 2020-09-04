module Language.PS.CST.Printers.Utils where

import Language.PS.CST.Types.Declaration (Declaration(..), Expr(..), InstanceBinding(..), LetBinding(..))
import Language.PS.CST.Types.Leafs (ModuleName(..), ProperName, ProperNameType_ConstructorName)
import Prelude
import Dodo (Doc, break, enclose, foldWithSeparator, softBreak, text)

import Data.Foldable (class Foldable)
import Data.List (List(..), (:))
import Data.List (fromFoldable) as List
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)

-- | >>> dquotes "·"
-- "·"
dquotes :: forall a. Doc a -> Doc a
dquotes = enclose dquote dquote

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
