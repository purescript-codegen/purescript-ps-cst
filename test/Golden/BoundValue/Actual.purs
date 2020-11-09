module Test.Golden.BoundValue.Actual where

import Language.PS.CST (Binder(..), Declaration(..), Expr(..), Guarded(..), Ident(..), LetBinding(..), Module(..), ProperName(..), Type(..), mkModuleName, nonQualifiedName, (====>>))

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty as NonEmpty
import Prelude

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ NonEmpty.cons' "BoundValue" []
  , imports: []
  , exports: []
  , declarations:
    [ DeclSignature
      { comments: Nothing
      , ident: Ident "myfunc"
      , type_: (TypeConstructor $ nonQualifiedName $ ProperName "Int") ====>> (TypeConstructor $ nonQualifiedName $ ProperName "Int")
      }
    , DeclValue
      { comments: Nothing
      , valueBindingFields:
        { name: Ident "myfunc"
        , binders: [BinderNumber (Left 1)]
        , guarded: Unconditional
            { expr: ExprLet
              { bindings:
                NonEmpty.cons'
                ( LetBindingName
                  { name: Ident "psModuleFile2"
                  , binders:
                    [ BinderConstructor { name: nonQualifiedName (ProperName "Path"), args: [BinderVar $ Ident "x"] }
                    ]
                  , guarded: Unconditional
                    { expr: ExprNumber (Left 1)
                    , whereBindings: []
                    }
                  }
                )
                []
              , body: (ExprIdent $ nonQualifiedName $ Ident "psModuleFile") `ExprApp` ExprNumber (Left 1)
              }
            , whereBindings: []
            }
        }
      }
    ]
  }
