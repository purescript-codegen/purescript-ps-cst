module Language.PS.CST.Printers.PrintImports where

import Prelude

import Language.PS.CST.Printers.Utils (printConstructors, printModuleName)
import Language.PS.CST.ReservedNames (appendUnderscoreIfReserved)

import Language.PS.CST.Types.Module (DataMembers(..), Import(..), ImportDecl(..))

import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import PrettyprinterRenderable (Doc, align, emptyDoc, flatAlt, group, nest, text, vcatOmittingEmpty, vsep, (<+>))
import PrettyprinterRenderable.Symbols.String (parens)
import PrettyprinterRenderable.Code.Purescript (encloseSep)

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

            onV2OnFlatten1Space = flatAlt (text "  ") (text " ")
          in
            group ( vcatOmittingEmpty
                    [ head
                    , nest 2 (onV2OnFlatten1Space <> align exports')
                    ]
                  )
            <> qualification'

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
