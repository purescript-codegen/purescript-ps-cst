module Test.Golden.Application.Actual where

import Language.PS.CST

import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty as NonEmpty
import Prelude (($))

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ NonEmpty.singleton "Foo"
  , imports: []
  , exports: []
  , declarations:
    [ DeclValue
      { comments: Nothing
      , valueBindingFields:
        { name: Ident "x"
        , binders: []
        , guarded: Unconditional
            { expr:
              (ExprIdent $ nonQualifiedName $ Ident "a")
              `ExprApp`
              (ExprIdent $ nonQualifiedName $ Ident "b")
              `ExprApp`
              (ExprIdent $ nonQualifiedName $ Ident "c")
              `ExprApp`
              (ExprIdent $ nonQualifiedName $ Ident "d")
              `ExprApp`
              (ExprIdent $ nonQualifiedName $ Ident "e")
              `ExprApp`
              (ExprIdent $ nonQualifiedName $ Ident "f")
              `ExprApp`
              (ExprIdent $ nonQualifiedName $ Ident "g")
              `ExprApp`
              (ExprIdent $ nonQualifiedName $ Ident "h")
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
            { expr:
              (ExprIdent $ nonQualifiedName $ Ident "a")
              `ExprApp`
              (ExprIdent $ nonQualifiedName $ Ident "b")
              `ExprApp`
              (
                (ExprIdent $ nonQualifiedName $ Ident "c")
                `ExprApp`
                (
                  (ExprIdent $ nonQualifiedName $ Ident "d")
                  `ExprApp`
                  (ExprIdent $ nonQualifiedName $ Ident "e")
                )
                `ExprApp`
                (ExprIdent $ nonQualifiedName $ Ident "f")
                `ExprApp`
                (ExprIdent $ nonQualifiedName $ Ident "g")
              )
              `ExprApp`
              (ExprIdent $ nonQualifiedName $ Ident "h")
            , whereBindings: []
            }
        }
      }
    ]
  }
