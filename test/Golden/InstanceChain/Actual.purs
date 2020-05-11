module Test.Golden.InstanceChain.Actual where

import Language.PS.AST.Sugar
import Language.PS.AST.Types

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.NonEmpty ((:|))
import Prelude (map, ($))

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ "InstanceChain" :| []
  , imports: []
  , exports: []
  , declarations:
    [ DeclInstanceChain
      (
        { head:
          { instName: Ident "fooBaz"
          , instConstraints: []
          , instClass: nonQualifiedName (ProperName "Foo")
          , instTypes: (nonQualifiedNameTypeConstructor "Baz") :| []
          }
        , body: []
        }
        :|
        [ { head:
            { instName: Ident "fooBaz"
            , instConstraints: []
            , instClass: nonQualifiedName (ProperName "Foo")
            , instTypes: (nonQualifiedNameTypeConstructor "Baz") :| []
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
            , instTypes: (nonQualifiedNameTypeConstructor "Cor" `TypeApp` nonQualifiedNameTypeConstructor "Int") :| [nonQualifiedNameTypeConstructor "Gar" `TypeApp` nonQualifiedNameTypeConstructor "Int" `TypeApp` nonQualifiedNameTypeConstructor "Boolean"]
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
    ]
  }
