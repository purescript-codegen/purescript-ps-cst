module Test.Golden.Imports.Actual where

import Data.Maybe (Maybe(..))
import Language.PS.AST.Types
import Language.PS.AST.Sugar (mkModuleName)
import Prelude (map, ($))

import Data.NonEmpty ((:|))

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
        [ ImportType (ProperName "Maybe") Nothing
        ]
      , qualification: Nothing
      }
    , ImportDecl
      { moduleName: mkModuleName $ "Prelude" :| []
      , names:
        [ ImportClass (ProperName "EuclideanRing")
        , ImportKind (ProperName "MyKind")
        , ImportTypeOp (OpName "<<<")
        , ImportType (ProperName "Ordering") (Just $ DataEnumerated $ map ProperName ["EQ", "GT", "LT"])
        , ImportType (ProperName "Unit") (Just DataAll)
        , ImportType (ProperName "Void") Nothing
        , ImportValue (Ident "compose")
        , ImportOp (OpName "&&")
        ]
      , qualification: Nothing
      }
    , ImportDecl
      { moduleName: mkModuleName $ "Data" :| ["Array"]
      , names:
        [ ImportValue (Ident "head")
        , ImportValue (Ident "tail")
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
