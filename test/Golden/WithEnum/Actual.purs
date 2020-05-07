module Test.Golden.WithEnum.Actual where

import Data.Either
import Data.Maybe
import Language.PS.AST
import Language.PS.AST.Sugar
import Language.PS.AST.Types
import Prelude

import Data.NonEmpty ((:|))

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ "WithEnum" :| []
  , imports: []
  , exports: []
  , declarations:
    [ DeclData
      ( DataHead
        { dataHdName: ProperName "Foo"
        , dataHdVars: []
        }
      )
      [ DataCtor { dataCtorName: ProperName "Bar", dataCtorFields: [] }
      , DataCtor { dataCtorName: ProperName "Baz", dataCtorFields: [] }
      ]
    ]
  }
