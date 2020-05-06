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
          wrapInParentheses :: Box -> Box
          wrapInParentheses x = text "(" <<>> x <<>> text ")"

          printConstructors :: Array (ProperName ProperNameType_ConstructorName) -> Box
          printConstructors = punctuateH left (text ", ") <<< map (text <<< unwrap)

          printImportName :: Import -> Box
          printImportName (ImportValue ident) = text $ unwrap ident
          printImportName (ImportOp valueOpName) = wrapInParentheses $ text $ unwrap valueOpName
          printImportName (ImportType properNameTypeName maybeDataMembers) =
            let
              printedProperNameTypeName :: Box
              printedProperNameTypeName = text $ unwrap properNameTypeName

              printedMaybeDataMembers :: Box
              printedMaybeDataMembers =
                case maybeDataMembers of
                  Nothing -> nullBox
                  (Just DataAll) -> text "(..)"
                  (Just (DataEnumerated constructors)) -> wrapInParentheses $ printConstructors constructors
            in printedProperNameTypeName <<>> printedMaybeDataMembers
          printImportName (ImportTypeOp opName) = text "type" <<+>> (wrapInParentheses $ text $ unwrap $ opName)
          printImportName (ImportClass properName) = text "class" <<+>> (text $ unwrap $ properName)
          printImportName (ImportKind properName) = text "kind" <<+>> (text $ unwrap $ properName)

          printedNamesColumn = vcat left $ map printImportName names
          commasColumn = vcat left $ [text "("] <> replicate (length names - 1) (text ",")
          printedNames = emptyBox 0 2 <<>> commasColumn <<+>> printedNamesColumn
        in
        head
        // printedNames
        // (emptyBox 0 2 <<>> text ")" <<+>> fromMaybe nullBox qualification')
