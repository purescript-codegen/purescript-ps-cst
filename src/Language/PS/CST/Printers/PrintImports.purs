module Language.PS.CST.Printers.PrintImports where

import Prelude (map, ($), (-), (<#>), (<>))
import Language.PS.CST.Types (DataMembers(..), Import(..), ImportDecl(..))
import Language.PS.CST.Printers.Utils (emptyColumn, emptyRow, printConstructors, printModuleName, textFromNewtype, twoSpaceIdentation, wrapInParentheses)
import Text.PrettyPrint.Boxes (Box, left, nullBox, text, vcat, vsep, (//), (<<+>>), (<<>>))
import Data.Foldable (length, null)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
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
        printImportName (ImportValue ident) = textFromNewtype ident
        printImportName (ImportOp valueOpName) = wrapInParentheses $ textFromNewtype valueOpName
        printImportName (ImportType properNameTypeName maybeDataMembers) =
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
        printImportName (ImportTypeOp opName) = text "type" <<+>> (wrapInParentheses $ textFromNewtype $ opName)
        printImportName (ImportClass properName) = text "class" <<+>> (textFromNewtype $ properName)
        printImportName (ImportKind properName) = text "kind" <<+>> (textFromNewtype $ properName)

        printedNamesColumn = vcat left $ map printImportName names

        commasColumn = vcat left $ [ text "(" ] <> replicate (length names - 1) (text ",")

        printedNames = twoSpaceIdentation <<>> commasColumn <<+>> printedNamesColumn
      in
        head
          // printedNames
          // (twoSpaceIdentation <<>> text ")" <<+>> fromMaybe nullBox qualification')
