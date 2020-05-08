module Test.Golden.DeclData.Actual where

import Language.PS.AST.Sugar (mkModuleName)
import Language.PS.AST.Types (DataCtor(..), DataHead(..), Declaration(..), Module(..), ProperName(..))
import Prelude (($))

import Data.NonEmpty ((:|))

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ "DeclData" :| []
  , imports: []
  , exports: []
  , declarations:
    [ DeclData
        { head: DataHead
            { dataHdName: ProperName "Foo"
            , dataHdVars: []
            }
        , constructors:
          [ DataCtor { dataCtorName: ProperName "Bar", dataCtorFields: [] }
          , DataCtor { dataCtorName: ProperName "Baz", dataCtorFields: [] }
          ]
        }
    ]
  }
