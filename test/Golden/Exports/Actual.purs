module Test.Golden.Exports.Actual where

import Data.Maybe (Maybe(..))
import Language.PS.AST.Types
import Language.PS.AST.Sugar (mkModuleName)
import Prelude (map, ($))

import Data.NonEmpty ((:|))

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ "Exports" :| []
  , imports: []
  , exports:
      [ ExportModule (mkModuleName $ "A" :| [])
      , ExportModule (mkModuleName $ "A" :| ["B"])
      , ExportClass (ProperName "A")
      , ExportKind (ProperName "A")
      , ExportTypeOp (OpName "||")
      , ExportType (ProperName "A") (Just $ DataEnumerated $ map ProperName ["B"])
      , ExportType (ProperName "A") (Just DataAll)
      , ExportType (ProperName "A") Nothing
      , ExportOp (OpName "||")
      , ExportValue (Ident "a")
      ]
  , declarations: []
  }
