module Language.PS.AST.Printers.Utils where

import Language.PS.AST.Types
import Prelude
import Text.PrettyPrint.Boxes

import Data.Array (cons, fromFoldable, null) as Array
import Data.Char.Unicode (isUpper)
import Data.Either (Either(..), either, fromRight)
import Data.Foldable (class Foldable, foldMap, intercalate, length, null)
import Data.List (List(..), (:))
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
