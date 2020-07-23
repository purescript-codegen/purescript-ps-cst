module Test.Golden.InstanceChain.Actual where

import Language.PS.CST

import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty as NonEmpty
import Prelude (($))

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
            , instTypes: NonEmpty.cons' (nonQualifiedNameTypeConstructor "Baz") []
            }
          , body: []
          }
          [ { head:
              { instName: Ident "fooBaz"
              , instConstraints: []
              , instClass: nonQualifiedName (ProperName "Foo")
              , instTypes: NonEmpty.cons' (nonQualifiedNameTypeConstructor "Baz") []
              }
            , body:
              [ InstanceBindingSignature { ident: Ident "foo", type_: numberType }
              , InstanceBindingName
                { name: Ident "foo"
                , binders: []
                , guarded: Unconditional
                  { expr: nonQualifiedExprIdent "append" `ExprApp` nonQualifiedExprIdent "foo" `ExprApp` nonQualifiedExprIdent "bar"
                  , whereBindings: []
                  }
                }
              , InstanceBindingSignature { ident: Ident "bar", type_: numberType }
              , InstanceBindingName
                { name: Ident "bar"
                , binders: []
                , guarded: Unconditional
                  { expr: nonQualifiedExprIdent "append" `ExprApp` (nonQualifiedExprIdent "foo" `ExprApp` nonQualifiedExprIdent "bar")
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
                ( nonQualifiedNameTypeConstructor "Cor"
                  `TypeApp`
                  nonQualifiedNameTypeConstructor "Int"
                )
                [ nonQualifiedNameTypeConstructor "Gar"
                  `TypeApp`
                  nonQualifiedNameTypeConstructor "Int"
                  `TypeApp`
                  nonQualifiedNameTypeConstructor "Boolean"
                ]
              }
            , body:
              [ InstanceBindingName
                { name: Ident "foo"
                , binders: []
                , guarded: Unconditional
                  { expr: nonQualifiedExprIdent "append"
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
