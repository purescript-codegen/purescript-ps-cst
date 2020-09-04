module Language.PS.CST.Printers.PrintModuleModuleNameAndExports where

import Prelude

import Language.PS.CST.Types.Module (DataMembers(..), Export(..))
import Language.PS.CST.Types.Leafs (ModuleName)
import Language.PS.CST.ReservedNames (appendUnderscoreIfReserved)
import Language.PS.CST.Printers.Utils

import Data.Newtype (unwrap)
import Data.Maybe (Maybe(..))
import Dodo
import Dodo.Common

printModuleModuleNameAndExports :: ModuleName -> Array Export -> Doc Void
printModuleModuleNameAndExports moduleName [] = text "module" <+> printModuleName moduleName <+> text "where"
printModuleModuleNameAndExports moduleName exports =
  let
    printedNames = pursParens $ foldWithSeparator leadingComma (map printExportName exports)
  in
  flexGroup $ text "module" <+> printModuleName moduleName <> softBreak <> (indent (printedNames <+> text "where"))

printExportName :: Export -> Doc Void
printExportName (ExportValue ident) = (text <<< appendUnderscoreIfReserved <<< unwrap) ident
printExportName (ExportOp valueOpName) = parens $ (text <<< appendUnderscoreIfReserved <<< unwrap) valueOpName
printExportName (ExportType properNameTypeName maybeDataMembers) =
  let
    printedProperNameTypeName :: Doc Void
    printedProperNameTypeName = (text <<< appendUnderscoreIfReserved <<< unwrap) properNameTypeName
    printedMaybeDataMembers :: Doc Void
    printedMaybeDataMembers = case maybeDataMembers of
      Nothing -> mempty
      (Just DataAll) -> text "(..)"
      (Just (DataEnumerated constructors)) -> parens $ printConstructors constructors
  in
    printedProperNameTypeName <> printedMaybeDataMembers
printExportName (ExportTypeOp opName) = text "type" <+> (parens $ (text <<< appendUnderscoreIfReserved <<< unwrap) $ opName)
printExportName (ExportClass properName) = text "class" <+> ((text <<< appendUnderscoreIfReserved <<< unwrap) $ properName)
printExportName (ExportKind properName) = text "kind" <+> ((text <<< appendUnderscoreIfReserved <<< unwrap) $ properName)
printExportName (ExportModule moduleName) = text "module" <+> printModuleName moduleName
