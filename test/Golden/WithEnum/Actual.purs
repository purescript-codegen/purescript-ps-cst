module Test.Golden.WithEnum.Actual where

import Language.PS.AST.Sugar (mkModuleName)
import Language.PS.AST.Types (DataCtor(..), DataHead(..), Declaration(..), Module(..), ProperName(..))
import Prelude (($))

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
