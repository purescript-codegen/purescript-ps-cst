module Test.Golden.Boolean.Actual where

import Language.PS.AST.Types

import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.NonEmpty ((:|))
import Language.PS.AST.Sugar (booleanType, mkModuleName, nonQualifiedName)
import Prelude (map, ($))

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ "Foo" :| []
  , imports: []
  , exports: []
  , declarations:
    [ DeclValue
      { name: Ident "x"
      , binders: []
      , guarded: Unconditional
          { expr: ExprBoolean true
          , bindings: []
          }
      }
    , DeclValue
      { name: Ident "x"
      , binders: []
      , guarded: Unconditional
          { expr: ExprBoolean true
          , bindings: []
          }
      }
    , DeclSignature
      { ident: Ident "y"
      , type_: booleanType
      }
    , DeclValue
      { name: Ident "y"
      , binders: []
      , guarded: Unconditional
          { expr: ExprBoolean true
          , bindings: []
          }
      }
    , DeclSignature
      { ident: Ident "z"
      , type_: booleanType ====>> booleanType
      }
    , DeclValue
      { name: Ident "z"
      , binders: [(BinderBoolean false)]
      , guarded: Unconditional
          { expr: ExprBoolean true
          , bindings: []
          }
      }
    ]
  }
