module Test.Golden.DeclData.Actual where

import Prelude

import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Language.PS.AST.Sugar (mkModuleName)
import Language.PS.AST.Types (DataCtor(..), DataHead(..), Declaration(..), Module(..), ProperName(..))

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ "DeclData" :| []
  , imports: []
  , exports: []
  , declarations:
    [ DeclData
      { comments: Nothing
      , head: DataHead
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
