module Language.PS.AST.Printers.Utils where

import Language.PS.AST.Types (ModuleName(..), ProperName, ProperNameType_ConstructorName)
import Prelude (const, identity, map, (#), (<<<), (>>>))
import Text.PrettyPrint.Boxes (Box, emptyBox, hsep, left, nullBox, punctuateH, text, vsep, (//), (<<>>))

import Data.Foldable (class Foldable)
import Data.List (List(..), (:))
import Data.List (fromFoldable) as List
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Debug.Trace (trace)

line :: ∀ f. Foldable f ⇒ f Box → Box
line = hsep 1 left

lines :: ∀ f. Foldable f ⇒ f Box → Box
lines = vsep 0 left

emptyRow :: Box
emptyRow = emptyBox 1 0

emptyColumn :: Box
emptyColumn = emptyBox 0 1

printModuleName :: ModuleName -> Box
printModuleName (ModuleName nonEmptyArray) =
  nonEmptyArray
    # map (unwrap >>> text)
    # punctuateH left (text ".")

wrapInParentheses :: Box -> Box
wrapInParentheses x = text "(" <<>> x <<>> text ")"

wrapInDoubleQuotes :: Box -> Box
wrapInDoubleQuotes x = text "\"" <<>> x <<>> text "\""

punctuateWithComma :: ∀ f. Foldable f ⇒ f Box → Box
punctuateWithComma = punctuateH left (text ", ")

textFromNewtype :: ∀ x. Newtype x String ⇒ x → Box
textFromNewtype = text <<< unwrap

twoSpaceIdentation :: Box
twoSpaceIdentation = emptyBox 0 2

printConstructors :: Array (ProperName ProperNameType_ConstructorName) -> Box
printConstructors = punctuateWithComma <<< map textFromNewtype

ifelse :: forall a. Boolean -> a -> a -> a
ifelse p a b = if p then a else b

foldWithPrev :: ∀ a b . (b -> Maybe a -> a -> b) -> b -> List a -> b
foldWithPrev _   default' Nil   = default'
foldWithPrev fun default' list = foo default' Nothing list
    where foo acc _    Nil     = acc
          foo acc prev (x : xs) = foo (fun acc prev x) (Just x) xs

maybeWrapInParentheses :: Boolean -> Box -> Box
maybeWrapInParentheses b = if b then wrapInParentheses else identity

printAndConditionallyAddNewlinesBetween :: ∀ a f . Foldable f => (a -> a -> Boolean) -> (a -> Box) -> f a -> Box
printAndConditionallyAddNewlinesBetween shouldBeNoNewlines print xs =
  let
    xs' :: List a
    xs' = List.fromFoldable xs

    foldDeclaration :: Box -> Maybe a -> a -> Box
    foldDeclaration accum Nothing current = accum // print current -- nullBox deactivates //
    foldDeclaration accum (Just prev) current = accum //
                                                if shouldBeNoNewlines prev current
                                                  then print current
                                                  else emptyRow // print current
   in
    foldWithPrev foldDeclaration nullBox xs'

traceId :: forall t2. t2 -> t2
traceId a = trace a (const a)
