module Language.PS.CST.Printers.PrintModuleModuleNameAndExports where

import Prelude (map, ($), (-), (<<<), (<>))

import Language.PS.CST.Types.Module (DataMembers(..), Export(..))
import Language.PS.CST.Types.Leafs (ModuleName)
import Language.PS.CST.ReservedNames (appendUnderscoreIfReserved)
import Language.PS.CST.Printers.Utils (printConstructors, printModuleName, twoSpaceIdentation, wrapInParentheses)

import Text.PrettyPrint.Boxes (Box, left, nullBox, text, vcat, (//), (<<+>>), (<<>>))
import Data.Newtype (unwrap)
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
printExportName (ExportValue ident) = (text <<< appendUnderscoreIfReserved <<< unwrap) ident
printExportName (ExportOp valueOpName) = wrapInParentheses $ (text <<< appendUnderscoreIfReserved <<< unwrap) valueOpName
printExportName (ExportType properNameTypeName maybeDataMembers) =
  let
    printedProperNameTypeName :: Box
    printedProperNameTypeName = (text <<< appendUnderscoreIfReserved <<< unwrap) properNameTypeName
    printedMaybeDataMembers :: Box
    printedMaybeDataMembers = case maybeDataMembers of
      Nothing -> nullBox
      (Just DataAll) -> text "(..)"
      (Just (DataEnumerated constructors)) -> wrapInParentheses $ printConstructors constructors
  in
    printedProperNameTypeName <<>> printedMaybeDataMembers
printExportName (ExportTypeOp opName) = text "type" <<+>> (wrapInParentheses $ (text <<< appendUnderscoreIfReserved <<< unwrap) $ opName)
printExportName (ExportClass properName) = text "class" <<+>> ((text <<< appendUnderscoreIfReserved <<< unwrap) $ properName)
printExportName (ExportKind properName) = text "kind" <<+>> ((text <<< appendUnderscoreIfReserved <<< unwrap) $ properName)
printExportName (ExportModule moduleName) = text "module" <<+>> printModuleName moduleName
