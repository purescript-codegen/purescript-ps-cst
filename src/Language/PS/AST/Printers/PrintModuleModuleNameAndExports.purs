module Language.PS.AST.Printers.PrintModuleModuleNameAndExports where

import Prelude (map, ($), (-), (<>))
import Language.PS.AST.Types (DataMembers(..), Export(..), ModuleName)
import Language.PS.AST.Printers.Utils (printConstructors, printModuleName, textFromNewtype, twoSpaceIdentation, wrapInParentheses)
import Text.PrettyPrint.Boxes (Box, left, nullBox, text, vcat, (//), (<<+>>), (<<>>))
import Data.Foldable (length)
import Data.Maybe (Maybe(..))
import Data.Unfoldable (replicate)

printModuleModuleNameAndExports :: ModuleName -> Array Export -> Box
printModuleModuleNameAndExports moduleName [] = text "module" <<+>> printModuleName moduleName <<+>> text "where"
printModuleModuleNameAndExports moduleName exports =
  let
    printedNamesColumn = vcat left $ map printExportName exports

    commasColumn = vcat left $ [ text "(" ] <> replicate (length exports - 1) (text ",")

    printedNames = twoSpaceIdentation <<>> commasColumn <<+>> printedNamesColumn
  in
    text "module" <<+>> printModuleName moduleName
      // printedNames
      // (twoSpaceIdentation <<>> text ")" <<+>> text "where")

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
