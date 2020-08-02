module Test.Golden.DeclData.Actual where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty as NonEmpty
import Language.PS.CST

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ NonEmpty.cons' "DeclData" []
  , imports: []
  , exports: []
  , declarations:
    [ DeclData
      { comments: Just $ BlockComments ["line1", "line2"]
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
