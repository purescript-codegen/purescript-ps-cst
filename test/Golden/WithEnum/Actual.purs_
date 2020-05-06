module Test.Golden.WithEnum.Actual where

import Data.Either
import Data.Maybe
import Language.PS.AST
import Prelude

import Data.Functor.Mu (roll)
import Data.List (fromFoldable) as List
import Language.PS.AST.Types

actualModule :: Module
actualModule = Module
  { moduleName: ModuleName "FooModuleName"
  , declarations: List.fromFoldable
    [ DeclData
      { dataDeclType: DeclDataTypeData
      , typeName: TypeName "Foo"
      , vars: []
      , constructors:
          [ { name: TypeName "Bar", types: [] }
          , { name: TypeName "Baz", types: [] }
          ]
      }
    ]
  }
  where
    qualifiedTypeNameFromPrelude typeName = { name: typeName, moduleName: Just $ ModuleName "Prelude" }
