module Test.Golden.Case.Actual where

import Language.PS.CST (Binder(..), Declaration(..), Expr(..), Guarded(..), Ident(..), LetBinding(..), Module(..), ProperName(..), mkModuleName, nonQualifiedName)

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty as NonEmpty
import Prelude

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
            { expr:
              ExprCase
              { head: NonEmpty.cons' (ExprIdent $ nonQualifiedName $ Ident "foo") []
              , branches:
                NonEmpty.cons'
                { binders: NonEmpty.cons' (BinderConstructor { name: nonQualifiedName (ProperName "Just"), args: [BinderVar (Ident "a")] }) []
                , body: Unconditional { expr: ExprBoolean true, whereBindings: [] }
                }
                [ { binders: NonEmpty.cons' (BinderConstructor { name: nonQualifiedName (ProperName "Nothing"), args: [] }) []
                  , body: Unconditional { expr: ExprBoolean false, whereBindings: [] }
                  }
                , { binders: NonEmpty.cons' (BinderString "FOO") []
                  , body: Unconditional
                    { expr:
                      ExprCase
                      { head:
                        NonEmpty.cons'
                        ( ExprIf
                          { cond: ExprBoolean true
                          , true_: ExprBoolean true
                          , false_: ExprBoolean false
                          }
                        )
                        [ ExprIf
                          { cond: ExprBoolean true
                          , true_: ExprBoolean true
                          , false_: ExprBoolean false
                          }
                        ]
                      , branches:
                        NonEmpty.cons'
                        { binders: NonEmpty.cons' (BinderConstructor { name: nonQualifiedName (ProperName "Just"), args: [BinderVar (Ident "a")] }) []
                        , body: Unconditional { expr: ExprBoolean true, whereBindings: [] }
                        }
                        []
                      }
                    , whereBindings: []
                    }
                  }
                , { binders: NonEmpty.cons' (BinderNumber (Right 1.1)) []
                  , body: Unconditional
                    { expr:
                      ExprCase
                      { head:
                        NonEmpty.cons'
                        ( ExprIf
                          { cond: ExprBoolean true
                          , true_: ExprBoolean true
                          , false_: ExprBoolean false
                          }
                        )
                        []
                      , branches:
                        NonEmpty.cons'
                        { binders: NonEmpty.cons' (BinderConstructor { name: nonQualifiedName (ProperName "Just"), args: [BinderVar (Ident "a")] }) []
                        , body: Unconditional { expr: ExprBoolean true, whereBindings: [] }
                        }
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
                , { binders: NonEmpty.singleton $ BinderNamed
                    { ident: Ident "a"
                    , binder: BinderConstructor
                      { name: nonQualifiedName (ProperName "Just")
                      , args: [BinderVar (Ident "a")]
                      }
                    }
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
