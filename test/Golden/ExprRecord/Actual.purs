module Test.Golden.ExprRecord.Actual where

import Language.PS.CST.Sugar
import Language.PS.CST.Types.Shared
import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty as NonEmpty
import Data.Tuple.Nested ((/\))
import Prelude

names :: Array String
names = ["Human", "Droid"]

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ NonEmpty.cons' "ExprRecord" []
  , imports: []
  , exports: []
  , declarations:
    [ DeclValue
      { comments: Nothing
      , valueBindingFields:
        { name: Ident "maybeFragments"
        , binders: []
        , guarded: Unconditional
            { expr:
              ExprRecord $ names <#> \name -> RecordField
                (Label $ "on" <> name)
                ( (ExprIdent $ nonQualifiedName $ Ident "pure")
                  `ExprApp`
                  (ExprConstructor $ nonQualifiedName $ ProperName "Nothing")
                )
            , whereBindings: []
            }
        }
      }
    ]
  }

