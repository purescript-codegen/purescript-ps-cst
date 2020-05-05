module Test.Golden.WithEnumWithRecord.Actual where

import Data.Either
import Data.Maybe
import Language.PS.AST
import Language.PS.AST.Types
import Prelude

import Data.Functor.Mu (roll)
import Data.List (fromFoldable) as List
import Data.Map (fromFoldable) as Map
import Data.Tuple (Tuple(..))

actualModule :: Module
actualModule = Module
  { moduleName: ModuleName "FooModuleName"
  , declarations: List.fromFoldable
    [ DeclData
      { dataDeclType: DeclDataTypeData
      , typeName: TypeName "Foo"
      , vars: []
      , constructors:
          [ { name: TypeName "Bar"
            , types:
              [ roll TypeBoolean
              , roll $ TypeRecord $ Row
                { labels: Map.fromFoldable [Tuple "foo" (roll TypeNumber)]
                , tail: Nothing
                }
              ]
            }
          , { name: TypeName "Baz"
            , types: [roll TypeBoolean]
            }
          ]
      }
    ]
  }
  where
    qualifiedTypeNameFromPrelude typeName = { name: typeName, moduleName: Just $ ModuleName "Prelude" }
