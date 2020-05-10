module Test.Golden.Application.Actual where

import Language.PS.AST.Types

import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.NonEmpty ((:|))
import Language.PS.AST.Sugar
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
            nonQualifiedExprIdent "a"
            `ExprApp`
            nonQualifiedExprIdent "b"
            `ExprApp`
            nonQualifiedExprIdent "c"
            `ExprApp`
            nonQualifiedExprIdent "d"
            `ExprApp`
            nonQualifiedExprIdent "e"
            `ExprApp`
            nonQualifiedExprIdent "f"
            `ExprApp`
            nonQualifiedExprIdent "g"
            `ExprApp`
            nonQualifiedExprIdent "h"
          , whereBindings: []
          }
      }
    , DeclValue
      { name: Ident "x"
      , binders: []
      , guarded: Unconditional
          { expr:
            nonQualifiedExprIdent "a"
            `ExprApp`
            nonQualifiedExprIdent "b"
            `ExprApp`
            (
              nonQualifiedExprIdent "c"
              `ExprApp`
              (
                nonQualifiedExprIdent "d"
                `ExprApp`
                nonQualifiedExprIdent "e"
              )
              `ExprApp`
              nonQualifiedExprIdent "f"
              `ExprApp`
              nonQualifiedExprIdent "g"
            )
            `ExprApp`
            nonQualifiedExprIdent "h"
          , whereBindings: []
          }
      }
    ]
  }
