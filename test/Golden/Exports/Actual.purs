module Test.Golden.Exports.Actual where

import Data.Maybe (Maybe(..))
import Language.PS.CST.Types
import Language.PS.CST.Sugar (mkModuleName)
import Prelude (map, ($))

import Data.Array.NonEmpty as NonEmpty

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ NonEmpty.cons' "Exports" []
  , imports: []
  , exports:
      [ ExportModule (mkModuleName $ NonEmpty.cons' "A" [])
      , ExportModule (mkModuleName $ NonEmpty.cons' "A" ["B"])
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
