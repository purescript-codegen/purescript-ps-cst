module Test.Golden.InstanceChain.Actual where

import Language.PS.CST (Declaration(..), Expr(..), Guarded(..), Ident(..), InstanceBinding(..), Module(..), ProperName(..), PSType(..), mkModuleName, nonQualifiedName, numberType)

import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty as NonEmpty
import Prelude

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ NonEmpty.cons' "InstanceChain" []
  , imports: []
  , exports: []
  , declarations:
    [ DeclInstanceChain
      { comments: Nothing
      , instances:
        (
          NonEmpty.cons'
          { head:
            { instName: Ident "fooBaz"
            , instConstraints: []
            , instClass: nonQualifiedName (ProperName "Foo")
            , instTypes: NonEmpty.cons' (TypeConstructor $ nonQualifiedName $ ProperName "Baz") []
            }
          , body: []
          }
          [ { head:
              { instName: Ident "fooBaz"
              , instConstraints: []
              , instClass: nonQualifiedName (ProperName "Foo")
              , instTypes: NonEmpty.cons' (TypeConstructor $ nonQualifiedName $ ProperName "Baz") []
              }
            , body:
              [ InstanceBindingSignature { ident: Ident "foo", type_: numberType }
              , InstanceBindingName
                { name: Ident "foo"
                , binders: []
                , guarded: Unconditional
                  { expr: (ExprIdent $ nonQualifiedName $ Ident "append") `ExprApp` (ExprIdent $ nonQualifiedName $ Ident "foo") `ExprApp` (ExprIdent $ nonQualifiedName $ Ident "bar")
                  , whereBindings: []
                  }
                }
              , InstanceBindingSignature { ident: Ident "bar", type_: numberType }
              , InstanceBindingName
                { name: Ident "bar"
                , binders: []
                , guarded: Unconditional
                  { expr: (ExprIdent $ nonQualifiedName $ Ident "append") `ExprApp` ((ExprIdent $ nonQualifiedName $ Ident "foo") `ExprApp` (ExprIdent $ nonQualifiedName $ Ident "bar"))
                  , whereBindings: []
                  }
                }
              ]
            }
          , { head:
              { instName: Ident "fooBaz"
              , instConstraints: []
              , instClass: nonQualifiedName (ProperName "Foo")
              , instTypes: NonEmpty.cons'
                ( (TypeConstructor $ nonQualifiedName $ ProperName "Cor")
                  `TypeApp`
                  (TypeConstructor $ nonQualifiedName $ ProperName "Int")
                )
                [ (TypeConstructor $ nonQualifiedName $ ProperName "Gar")
                  `TypeApp`
                  (TypeConstructor $ nonQualifiedName $ ProperName "Int")
                  `TypeApp`
                  (TypeConstructor $ nonQualifiedName $ ProperName "Boolean")
                ]
              }
            , body:
              [ InstanceBindingName
                { name: Ident "foo"
                , binders: []
                , guarded: Unconditional
                  { expr: ExprIdent $ nonQualifiedName $ Ident "append"
                  , whereBindings: []
                  }
                }
              ]
            }
          ]
        )
      }
    ]
  }
