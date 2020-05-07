module Test.Golden.Exports.Actual where

import Data.Maybe (Maybe(..))
import Language.PS.AST.Types (DataMembers(..), Export(..), Module(..))
import Language.PS.AST.Sugar (mkModuleName)
import Prelude (map, ($))

import Data.Newtype (wrap)
import Data.NonEmpty ((:|))

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ "Exports" :| []
  , imports: []
  , exports:
      [ ExportModule (mkModuleName $ "A" :| [])
      , ExportModule (mkModuleName $ "A" :| ["B"])
      , ExportClass (wrap "A")
      , ExportKind (wrap "A")
      , ExportTypeOp (wrap "||")
      , ExportType (wrap "A") (Just $ DataEnumerated $ map wrap ["B"])
      , ExportType (wrap "A") (Just DataAll)
      , ExportType (wrap "A") Nothing
      , ExportOp (wrap "||")
      , ExportValue (wrap "a")
      ]
  , declarations: []
  }
