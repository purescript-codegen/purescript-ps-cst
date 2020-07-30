module Test.Golden.MultilinePatternMatchingInLet.Actual where

import Language.PS.CST

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty as NonEmpty
import Prelude (($))

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ NonEmpty.cons' "MultilinePatternMatchingInLet" []
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
                ( LetBindingSignature
                  { ident: Ident "psModuleFile"
                  , type_: (TypeConstructor $ nonQualifiedName $ ProperName "ModulePath") ====>> (TypeConstructor $ nonQualifiedName $ ProperName "Int")
                  }
                )
                [ LetBindingName
                  { name: Ident "psModuleFile"
                  , binders:
                    [ BinderConstructor { name: nonQualifiedName (ProperName "Path"), args: [] }
                    ]
                  , guarded: Unconditional
                    { expr: ExprNumber (Left 1)
                    , whereBindings: []
                    }
                  }
                , LetBindingName
                  { name: Ident "psModuleFile"
                  , binders:
                    [ BinderConstructor { name: nonQualifiedName (ProperName "Name"), args: [] }
                    ]
                  , guarded: Unconditional
                    { expr: ExprNumber (Left 2)
                    , whereBindings: []
                    }
                  }
                ]
              , body: (ExprIdent $ nonQualifiedName $ Ident "psModuleFile") `ExprApp` ExprNumber (Left 1)
              }
            , whereBindings: []
            }
        }
      }
    ]
  }
