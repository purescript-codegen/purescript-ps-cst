module Test.Golden.Exports.Actual where

import Language.PS.CST (DataMembers(..), Export(..), Ident(..), Module(..), OpName(..), ProperName(..), mkModuleName)

import Prelude
import Data.Maybe (Maybe(..))
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
