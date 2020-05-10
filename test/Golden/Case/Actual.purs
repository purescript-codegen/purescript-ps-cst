module Test.Golden.Case.Actual where

import Language.PS.AST.Sugar
import Language.PS.AST.Types

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.NonEmpty ((:|))
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
          { expr:
            ExprCase
            { head: nonQualifiedExprIdent "foo" :| []
            , branches:
              { binders: BinderConstructor { name: nonQualifiedName (ProperName "Just"), args: [BinderVar (Ident "a")] } :| []
              , body: Unconditional { expr: ExprBoolean true, whereBindings: [] }
              }
              :|
              [ { binders: BinderConstructor { name: nonQualifiedName (ProperName "Nothing"), args: [] } :| []
                , body: Unconditional { expr: ExprBoolean false, whereBindings: [] }
                }
              , { binders: BinderString "FOO" :| []
                , body: Unconditional { expr: ExprBoolean true, whereBindings: [] }
                }
              , { binders: BinderNumber (Right 1.1) :| []
                , body: Unconditional
                  { expr:
                    ExprCase
                    { head:
                      ExprIf
                      { cond: ExprBoolean true
                      , true_: ExprBoolean true
                      , false_: ExprBoolean false
                      }
                      :|
                      []
                    , branches:
                      { binders: BinderConstructor { name: nonQualifiedName (ProperName "Just"), args: [BinderVar (Ident "a")] } :| []
                      , body: Unconditional { expr: ExprBoolean true, whereBindings: [] }
                      }
                      :|
                      []
                    }
                  , whereBindings: []
                  }
                }
              , { binders: BinderNamed
                  { ident: Ident "a"
                  , binder: BinderConstructor
                    { name: nonQualifiedName (ProperName "Just")
                    , args: [BinderVar (Ident "a")]
                    }
                  } :| []
                , body: Unconditional { expr: ExprBoolean false, whereBindings: [] }
                }
              ]
            }
          , whereBindings: []
          }
      }
    ]
  }
