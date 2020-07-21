module Language.PS.CST.Printers.PrintImports where

import Prelude

import Language.PS.CST.Printers.Utils (emptyColumn, emptyRow, printConstructors, printModuleName, twoSpaceIdentation, wrapInParentheses)
import Language.PS.CST.ReservedNames (appendUnderscoreIfReserved)
import Language.PS.CST.Types (DataMembers(..), Import(..), ImportDecl(..))

import Text.PrettyPrint.Boxes (Box, left, nullBox, text, vcat, vsep, (//), (<<+>>), (<<>>))
import Data.Foldable (length, null)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Unfoldable (replicate)

printImports :: Array ImportDecl -> Box
printImports [] = nullBox
printImports imports =
  emptyRow
    // (vsep 0 left $ map printImport imports)

printImport :: ImportDecl -> Box
printImport (ImportDecl { moduleName, names, qualification }) =
  let
    head = text "import" <<+>> printModuleName moduleName

    qualification' = qualification <#> (\qualificationModuleName -> text "as" <<+>> printModuleName qualificationModuleName)

    prependSpace x = emptyColumn <<>> x
  in
    if null names then
      head <<>> maybe nullBox prependSpace qualification' -- in one line
    else
      let
        printImportName :: Import -> Box
        printImportName (ImportValue ident) = (text <<< appendUnderscoreIfReserved <<< unwrap) ident
        printImportName (ImportOp valueOpName) = wrapInParentheses $ (text <<< appendUnderscoreIfReserved <<< unwrap) valueOpName
        printImportName (ImportType properNameTypeName maybeDataMembers) =
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
        printImportName (ImportTypeOp opName) = text "type" <<+>> (wrapInParentheses $ (text <<< appendUnderscoreIfReserved <<< unwrap) $ opName)
        printImportName (ImportClass properName) = text "class" <<+>> ((text <<< appendUnderscoreIfReserved <<< unwrap) $ properName)
        printImportName (ImportKind properName) = text "kind" <<+>> ((text <<< appendUnderscoreIfReserved <<< unwrap) $ properName)

        printedNamesColumn = vcat left $ map printImportName names

        commasColumn = vcat left $ [ text "(" ] <> replicate (length names - 1) (text ",")

        printedNames = twoSpaceIdentation <<>> commasColumn <<+>> printedNamesColumn
      in
        head
          // printedNames
          // (twoSpaceIdentation <<>> text ")" <<+>> fromMaybe nullBox qualification')
