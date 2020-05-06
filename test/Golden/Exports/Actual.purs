module Test.Golden.Exports.Actual where

import Data.Either
import Data.Maybe
import Language.PS.AST
import Language.PS.AST.Types
import Language.PS.AST.Sugar
import Prelude

import Data.Functor.Mu (roll)
import Data.List (fromFoldable) as List
import Data.Newtype (wrap)
import Data.NonEmpty (NonEmpty(..), (:|))

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
  }
