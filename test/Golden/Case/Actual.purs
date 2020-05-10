module Test.Golden.Case.Actual where

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
          , whereBindings: []
          }
      }
    ]
  }
