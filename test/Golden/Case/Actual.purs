module Test.Golden.Case.Actual where

import Language.PS.CST.Sugar (mkModuleName, nonQualifiedExprIdent, nonQualifiedName)
import Language.PS.CST.Types (Binder(..), Declaration(..), Expr(..), Guarded(..), Ident(..), LetBinding(..), Module(..), ProperName(..))

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
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
                        [ ExprIf
                          { cond: ExprBoolean true
                          , true_: ExprBoolean true
                          , false_: ExprBoolean false
                          }
                        ]
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
                    , whereBindings:
                      [ LetBindingName
                          { name: Ident "foo"
                          , binders: []
                          , guarded: Unconditional
                            { expr: ExprNumber (Left 1)
                            , whereBindings: []
                            }
                          }
                      ]
                    }
                  }
                , { binders: BinderNamed
                    { ident: Ident "a"
                    , binder: BinderConstructor
                      { name: nonQualifiedName (ProperName "Just")
                      , args: [BinderVar (Ident "a")]
                      }
                    } :| []
                  , body: Unconditional
                    { expr: ExprBoolean false
                    , whereBindings: []
                    }
                  }
                ]
              }
            , whereBindings: []
            }
        }
      }
    ]
  }
