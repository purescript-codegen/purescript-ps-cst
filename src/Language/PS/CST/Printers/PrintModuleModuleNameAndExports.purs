module Language.PS.CST.Printers.PrintModuleModuleNameAndExports where

import Prelude

import Language.PS.CST.Types.Module (DataMembers(..), Export(..))
import Language.PS.CST.Types.Leafs (ModuleName)
import Language.PS.CST.ReservedNames (appendUnderscoreIfReserved)
import Language.PS.CST.Printers.Utils

import Data.Newtype (unwrap)
import Data.Foldable (length)
import Data.Maybe (Maybe(..))
import Data.Unfoldable (replicate)
import Text.Pretty
import Text.Pretty.Code.Purescript (encloseSep)
import Text.Pretty.Symbols.String
import Data.Container.Class

printModuleModuleNameAndExports :: ModuleName -> Array Export -> Doc String
printModuleModuleNameAndExports moduleName [] = text "module" <+> printModuleName moduleName <+> text "where"
printModuleModuleNameAndExports moduleName exports =
  let
    printedNames = encloseSep (text "(") (text ")") (text ", ") (map printExportName exports)

    prefer1SpaceOr2OnGroup = flatAlt (text "  ") (text " ")
  in
    text "module" <+> printModuleName moduleName <> line' <> (nest 2 (prefer1SpaceOr2OnGroup <> printedNames <+> text "where"))

printExportName :: Export -> Doc String
printExportName (ExportValue ident) = (text <<< appendUnderscoreIfReserved <<< unwrap) ident
printExportName (ExportOp valueOpName) = parens $ (text <<< appendUnderscoreIfReserved <<< unwrap) valueOpName
printExportName (ExportType properNameTypeName maybeDataMembers) =
  let
    printedProperNameTypeName :: Doc String
    printedProperNameTypeName = (text <<< appendUnderscoreIfReserved <<< unwrap) properNameTypeName
    printedMaybeDataMembers :: Doc String
    printedMaybeDataMembers = case maybeDataMembers of
      Nothing -> emptyDoc
      (Just DataAll) -> text "(..)"
      (Just (DataEnumerated constructors)) -> parens $ printConstructors constructors
  in
    printedProperNameTypeName <> printedMaybeDataMembers
printExportName (ExportTypeOp opName) = text "type" <+> (parens $ (text <<< appendUnderscoreIfReserved <<< unwrap) $ opName)
printExportName (ExportClass properName) = text "class" <+> ((text <<< appendUnderscoreIfReserved <<< unwrap) $ properName)
printExportName (ExportKind properName) = text "kind" <+> ((text <<< appendUnderscoreIfReserved <<< unwrap) $ properName)
printExportName (ExportModule moduleName) = text "module" <+> printModuleName moduleName
