module Language.PS.AST.Printers.PrintImports where

import Prelude
import Language.PS.AST.Types
import Language.PS.AST.Printers.Utils
import Text.PrettyPrint.Boxes

import Data.Array (cons, fromFoldable, null) as Array
import Data.Char.Unicode (isUpper)
import Data.Either (Either(..), fromRight)
import Data.Foldable (class Foldable, foldMap, intercalate, length, null)
import Data.List (List(..))
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

printedImports :: Array ImportDecl -> Box
printedImports [] = nullBox
printedImports imports =
  emptyRow
  // (vsep 0 left $ map printImport imports)

printImport :: ImportDecl -> Box
printImport (ImportDecl { moduleName, names, qualification }) =
  let
    head = text "import" <<+>> printModuleName moduleName
    qualification' = qualification <#> (\qualificationModuleName -> text "as" <<+>> printModuleName qualificationModuleName)
    prependSpace x = emptyColumn <<>> x
  in
    if null names
      then head <<>> maybe nullBox prependSpace qualification' -- in one line
      else
        let
          printImportName :: Import -> Box
          printImportName (ImportValue ident) = textFromNewtype ident
          printImportName (ImportOp valueOpName) = wrapInParentheses $ textFromNewtype valueOpName
          printImportName (ImportType properNameTypeName maybeDataMembers) =
            let
              printedProperNameTypeName :: Box
              printedProperNameTypeName = textFromNewtype properNameTypeName

              printedMaybeDataMembers :: Box
              printedMaybeDataMembers =
                case maybeDataMembers of
                  Nothing -> nullBox
                  (Just DataAll) -> text "(..)"
                  (Just (DataEnumerated constructors)) -> wrapInParentheses $ printConstructors constructors
            in printedProperNameTypeName <<>> printedMaybeDataMembers
          printImportName (ImportTypeOp opName) = text "type" <<+>> (wrapInParentheses $ textFromNewtype $ opName)
          printImportName (ImportClass properName) = text "class" <<+>> (textFromNewtype $ properName)
          printImportName (ImportKind properName) = text "kind" <<+>> (textFromNewtype $ properName)

          printedNamesColumn = vcat left $ map printImportName names
          commasColumn = vcat left $ [text "("] <> replicate (length names - 1) (text ",")
          printedNames = twoSpaceIdentation <<>> commasColumn <<+>> printedNamesColumn
        in
        head
        // printedNames
        // (twoSpaceIdentation <<>> text ")" <<+>> fromMaybe nullBox qualification')
