module Test.Golden.Boolean.Actual where

import Language.PS.CST

import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty as NonEmpty
import Prelude (($))

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ NonEmpty.cons' "Foo" []
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
