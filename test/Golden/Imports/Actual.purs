module Test.Golden.Imports.Actual where

import Data.Maybe (Maybe(..))
import Language.PS.CST.Types
import Language.PS.CST.Sugar (mkModuleName)
import Prelude (map, ($))

import Data.Array.NonEmpty as NonEmpty

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ NonEmpty.cons' "Foo" []
  , imports:
    [ ImportDecl
      { moduleName: mkModuleName $ NonEmpty.cons' "Prelude" []
      , names: []
      , qualification: Nothing
      }
    , ImportDecl
      { moduleName: mkModuleName $ NonEmpty.cons' "Data" ["Maybe"]
      , names:
        [ ImportType (ProperName "Maybe") Nothing
        ]
      , qualification: Nothing
      }
    , ImportDecl
      { moduleName: mkModuleName $ NonEmpty.cons' "Prelude" []
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
      { moduleName: mkModuleName $ NonEmpty.cons' "Data" ["Array"]
      , names:
        [ ImportValue (Ident "head")
        , ImportValue (Ident "tail")
        ]
      , qualification: Just $ mkModuleName $ NonEmpty.cons' "Array" []
      }
    , ImportDecl
      { moduleName: mkModuleName $ NonEmpty.cons' "Data" ["List"]
      , names: []
      , qualification: Just $ mkModuleName $ NonEmpty.cons' "My" ["Data", "List"]
      }
    ]
  , exports: []
  , declarations: []
  }
