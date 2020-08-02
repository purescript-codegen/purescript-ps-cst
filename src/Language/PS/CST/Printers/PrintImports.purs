module Language.PS.CST.Printers.PrintImports where

import Prelude

import Language.PS.CST.Printers.Utils
import Language.PS.CST.ReservedNames (appendUnderscoreIfReserved)

import Language.PS.CST.Types.Module (DataMembers(..), Import(..), ImportDecl(..))

import Data.Array as Array
import Data.Foldable (length, null)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Unfoldable (replicate)
import Text.Pretty
import Text.Pretty as Pretty
import Text.Pretty.Symbols.String hiding (space)
import Text.Pretty.Code.Purescript (tupled, encloseSep)
import Data.Container.Class

printImports :: Array ImportDecl -> Doc String
printImports imports = vsep $ map printImport imports

printImport :: ImportDecl -> Doc String
printImport (ImportDecl { moduleName, names, qualification }) =
  let
    head = text "import" <+> printModuleName moduleName

    qualification' :: Doc String
    qualification' = maybe emptyDoc (\qualificationModuleName -> text " as" <+> printModuleName qualificationModuleName) qualification
  in
    case names of
         [] -> head <> qualification' -- in one line
         _ ->
          let
            exports :: Array (Doc String)
            exports = map printImportName names

            exports' = encloseSep (text "(") (text ")") (text ", ") exports

            prefer1SpaceOr2OnGroup = flatAlt (text "  ") (text " ")
          in group (head <> line' <> nest 2 (prefer1SpaceOr2OnGroup <> align exports')) <> qualification'

printImportName :: Import -> Doc String
printImportName (ImportValue ident) = (text <<< appendUnderscoreIfReserved <<< unwrap) ident
printImportName (ImportOp valueOpName) = parens $ (text <<< appendUnderscoreIfReserved <<< unwrap) valueOpName
printImportName (ImportType properNameTypeName maybeDataMembers) =
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
printImportName (ImportTypeOp opName) = text "type" <+> (parens $ (text <<< appendUnderscoreIfReserved <<< unwrap) $ opName)
printImportName (ImportClass properName) = text "class" <+> ((text <<< appendUnderscoreIfReserved <<< unwrap) $ properName)
printImportName (ImportKind properName) = text "kind" <+> ((text <<< appendUnderscoreIfReserved <<< unwrap) $ properName)
