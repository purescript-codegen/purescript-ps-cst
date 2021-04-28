module Language.PS.CST.Printers.PrintImports where

import Prelude

import Language.PS.CST.Printers.Utils (parens, printConstructors, printModuleName)
import Language.PS.CST.ReservedNames (appendUnderscoreIfReserved)

import Language.PS.CST.Types.Module (DataMembers(..), Import(..), ImportDecl(..))

import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Dodo (Doc, alignCurrentColumn, flexGroup, foldWithSeparator, indent, lines, paragraph, text, (<+>))
import Dodo.Common (leadingComma, pursParens)

printImports :: Array ImportDecl -> Doc Void
printImports imports = lines $ map printImport imports

printImport :: ImportDecl -> Doc Void
printImport (ImportDecl { moduleName, names, qualification }) =
  let
    head = text "import" <+> printModuleName moduleName

    qualification' :: Doc Void
    qualification' = maybe mempty (\qualificationModuleName -> text " as" <+> printModuleName qualificationModuleName) qualification
  in
    case names of
         [] -> head <> qualification' -- in one spaceBreak
         _ ->
          let
            exports :: Array (Doc Void)
            exports = map printImportName names

            exports' = pursParens $ foldWithSeparator leadingComma exports
          in
            flexGroup ( paragraph
                    [ head
                    , indent (alignCurrentColumn exports')
                    ]
                  )
            <> qualification'

printImportName :: Import -> Doc Void
printImportName (ImportValue ident) = (text <<< appendUnderscoreIfReserved <<< unwrap) ident
printImportName (ImportOp valueOpName) = parens $ (text <<< appendUnderscoreIfReserved <<< unwrap) valueOpName
printImportName (ImportType properNameTypeConstructor maybeDataMembers) =
  let
    printedProperNameTypeConstructor :: Doc Void
    printedProperNameTypeConstructor = (text <<< appendUnderscoreIfReserved <<< unwrap) properNameTypeConstructor

    printedMaybeDataMembers :: Doc Void
    printedMaybeDataMembers = case maybeDataMembers of
      Nothing -> mempty
      (Just DataAll) -> text "(..)"
      (Just (DataEnumerated constructors)) -> parens $ printConstructors constructors
  in
    printedProperNameTypeConstructor <> printedMaybeDataMembers
printImportName (ImportTypeOp opName) = text "type" <+> (parens $ (text <<< appendUnderscoreIfReserved <<< unwrap) $ opName)
printImportName (ImportClass properName) = text "class" <+> ((text <<< appendUnderscoreIfReserved <<< unwrap) $ properName)
printImportName (ImportKind properName) = text "kind" <+> ((text <<< appendUnderscoreIfReserved <<< unwrap) $ properName)
