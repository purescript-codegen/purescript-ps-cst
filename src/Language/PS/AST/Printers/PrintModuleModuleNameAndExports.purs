module Language.PS.AST.Printers.PrintModuleModuleNameAndExports where

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

printModuleModuleNameAndExports :: ModuleName -> Array Export -> Box
printModuleModuleNameAndExports moduleName [] = text "module" <<+>> printModuleName moduleName <<+>> text "where"
printModuleModuleNameAndExports moduleName exports =
  let
    printExportName :: Export -> Box
    printExportName (ExportValue ident) = textFromNewtype ident
    printExportName (ExportOp valueOpName) = wrapInParentheses $ textFromNewtype valueOpName
    printExportName (ExportType properNameTypeName maybeDataMembers) =
      let
        printedProperNameTypeName :: Box
        printedProperNameTypeName = textFromNewtype properNameTypeName

        printedMaybeDataMembers :: Box
        printedMaybeDataMembers = case maybeDataMembers of
          Nothing -> nullBox
          (Just DataAll) -> text "(..)"
          (Just (DataEnumerated constructors)) -> wrapInParentheses $ printConstructors constructors
      in
        printedProperNameTypeName <<>> printedMaybeDataMembers
    printExportName (ExportTypeOp opName) = text "type" <<+>> (wrapInParentheses $ textFromNewtype $ opName)
    printExportName (ExportClass properName) = text "class" <<+>> (textFromNewtype $ properName)
    printExportName (ExportKind properName) = text "kind" <<+>> (textFromNewtype $ properName)
    printExportName (ExportModule moduleName) = text "module" <<+>> printModuleName moduleName

    printedNamesColumn = vcat left $ map printExportName exports

    commasColumn = vcat left $ [ text "(" ] <> replicate (length exports - 1) (text ",")

    printedNames = twoSpaceIdentation <<>> commasColumn <<+>> printedNamesColumn
  in
    text "module" <<+>> printModuleName moduleName
      // printedNames
      // (twoSpaceIdentation <<>> text ")" <<+>> text "where")
