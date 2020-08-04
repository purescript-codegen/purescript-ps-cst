module Language.PS.CST.Printers.Utils where

import Data.Foldable (class Foldable)
import Data.List (List(..), (:))
import Data.List (fromFoldable) as List
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Language.PS.CST.Types.Leafs
import Language.PS.CST.Types.Declaration
import Prelude
import Text.Pretty
import Text.Pretty.Symbols.String
import Data.Container.Class

printModuleName :: ModuleName -> Doc String
printModuleName (ModuleName nonEmptyArray) = concatWithNonEmpty (surround dot) $ map (unwrap >>> text) (nonEmptyArray)

punctuateWithComma :: forall f . Container f => Foldable f => f (Doc String) -> Doc String
punctuateWithComma = concatWith (surround (text ", "))

twoSpaceIdentation :: Doc String
twoSpaceIdentation = text "  "

printConstructors :: Array (ProperName ProperNameType_ConstructorName) -> Doc String
printConstructors = punctuateWithComma <<< map (text <<< unwrap)

foldWithPrev :: ∀ a b . (b -> Maybe a -> a -> b) -> b -> List a -> b
foldWithPrev _   default' Nil   = default'
foldWithPrev fun default' list = foo default' Nothing list
    where foo acc _    Nil     = acc
          foo acc prev (x : xs) = foo (fun acc prev x) (Just x) xs

maybeWrapInParentheses :: Boolean -> Doc String -> Doc String
maybeWrapInParentheses b = if b then parens else identity

printAndConditionallyAddNewlinesBetween :: ∀ a f . Foldable f => (a -> a -> Boolean) -> (a -> Doc String) -> f a -> Doc String
printAndConditionallyAddNewlinesBetween shouldBeNoNewlines print =
  let
    foldDeclaration :: Doc String -> Maybe a -> a -> Doc String
    foldDeclaration accum Nothing current = print current
    foldDeclaration accum (Just prev) current = if shouldBeNoNewlines prev current
                                                  then accum <> line' <> print current
                                                  else accum <> line' <> hardline <> (print current)
   in
    foldWithPrev foldDeclaration emptyDoc <<< List.fromFoldable

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
