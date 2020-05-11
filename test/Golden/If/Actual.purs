module Test.Golden.If.Actual where

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
