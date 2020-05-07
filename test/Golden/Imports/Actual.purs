module Test.Golden.Imports.Actual where

import Data.Either
import Data.Maybe
import Language.PS.AST
import Language.PS.AST.Types
import Language.PS.AST.Sugar
import Prelude

import Data.Functor.Mu (roll)
import Data.List (fromFoldable) as List
import Data.Newtype (wrap)
import Data.NonEmpty (NonEmpty(..), (:|))

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ "Foo" :| []
  , imports:
    [ ImportDecl
      { moduleName: mkModuleName $ "Prelude" :| []
      , names: []
      , qualification: Nothing
      }
    , ImportDecl
      { moduleName: mkModuleName $ "Data" :| ["Maybe"]
      , names:
        [ ImportType (wrap "Maybe") Nothing
        ]
      , qualification: Nothing
      }
    , ImportDecl
      { moduleName: mkModuleName $ "Prelude" :| []
      , names:
        [ ImportClass (wrap "EuclideanRing")
        , ImportKind (wrap "MyKind")
        , ImportTypeOp (wrap "<<<")
        , ImportType (wrap "Ordering") (Just $ DataEnumerated $ map wrap ["EQ", "GT", "LT"])
        , ImportType (wrap "Unit") (Just DataAll)
        , ImportType (wrap "Void") Nothing
        , ImportValue (wrap "compose")
        , ImportOp (wrap "&&")
        ]
      , qualification: Nothing
      }
    , ImportDecl
      { moduleName: mkModuleName $ "Data" :| ["Array"]
      , names:
        [ ImportValue (wrap "head")
        , ImportValue (wrap "tail")
        ]
      , qualification: Just $ mkModuleName $ "Array" :| []
      }
    , ImportDecl
      { moduleName: mkModuleName $ "Data" :| ["List"]
      , names: []
      , qualification: Just $ mkModuleName $ "My" :| ["Data", "List"]
      }
    ]
  , exports: []
  , declarations: []
  }
