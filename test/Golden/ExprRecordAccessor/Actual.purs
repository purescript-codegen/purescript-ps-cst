module Test.Golden.ExprRecordAccessor.Actual where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmpty
import Data.Maybe (Maybe(..))
import Language.PS.CST (Declaration(..), Expr(..), Guarded(..), Ident(..), Label(..), Module(..), QualifiedName(..), mkModuleName)

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ NonEmpty.cons' "ExprRecordAccessor" []
  , imports: []
  , exports: []
  , declarations:
    [ accessorDeclValue "foo" $ NonEmpty.singleton "a"
    , accessorDeclValue "bar" $ NonEmpty.singleton "A"
    , accessorDeclValue "baz" $ NonEmpty.cons' "a" [ "B", "c" ]
    ]
  }

accessorDeclValue :: String -> NonEmptyArray String -> Declaration
accessorDeclValue name path =
  DeclValue
  { comments: Nothing
  , valueBindingFields:
    { name: Ident name
    , binders: []
    , guarded: Unconditional
      { expr:
        ExprRecordAccessor
        { recExpr: ExprIdent $ QualifiedName
          { qualModule: Nothing
          , qualName: Ident "rec"
          }
        , recPath: Label <$> path
        }
      , whereBindings: []
      }
    }
  }
