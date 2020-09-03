module Test.Golden.If.Actual where

import Language.PS.CST (Declaration(..), Expr(..), Guarded(..), Ident(..), Module(..), mkModuleName)

import Prelude
import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty as NonEmpty

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
              ExprIf
              { cond:
                ExprIf
                { cond: ExprBoolean true
                , true_: ExprBoolean true
                , false_: ExprBoolean false
                }
              , true_:
                ExprIf
                { cond: ExprBoolean true
                , true_: ExprBoolean true
                , false_: ExprBoolean false
                }
              , false_:
                ExprIf
                { cond: ExprBoolean true
                , true_: ExprBoolean true
                , false_: ExprBoolean false
                }
              }
            , whereBindings: []
            }
        }
      }
    ]
  }
