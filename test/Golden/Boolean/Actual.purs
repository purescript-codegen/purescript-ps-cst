module Test.Golden.Boolean.Actual where

import Language.PS.AST.Types

import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Language.PS.AST.Sugar (booleanType, mkModuleName)
import Prelude (($))

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ "Foo" :| []
  , imports: []
  , exports: []
  , declarations:
    [ DeclValue
      { comments: Nothing
      , valueBindingFields:
        { name: Ident "x"
        , binders: []
        , guarded: Unconditional
            { expr: ExprBoolean true
            , whereBindings: []
            }
        }
      }
    , DeclValue
      { comments: Nothing
      , valueBindingFields:
        { name: Ident "x"
        , binders: []
        , guarded: Unconditional
            { expr: ExprBoolean true
            , whereBindings: []
            }
        }
      }
    , DeclSignature
      { comments: Nothing
      , ident: Ident "y"
      , type_: booleanType
      }
    , DeclValue
      { comments: Nothing
      , valueBindingFields:
        { name: Ident "y"
        , binders: []
        , guarded: Unconditional
            { expr: ExprBoolean true
            , whereBindings: []
            }
        }
      }
    , DeclSignature
      { comments: Nothing
      , ident: Ident "z"
      , type_: booleanType ====>> booleanType
      }
    , DeclValue
      { comments: Nothing
      , valueBindingFields:
        { name: Ident "z"
        , binders: [(BinderBoolean false)]
        , guarded: Unconditional
            { expr: ExprBoolean true
            , whereBindings: []
            }
        }
      }
    ]
  }
