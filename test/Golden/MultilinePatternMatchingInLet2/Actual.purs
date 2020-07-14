module Test.Golden.MultilinePatternMatchingInLet2.Actual where

import Language.PS.CST.Sugar (mkModuleName, nonQualifiedExprIdent, nonQualifiedName, nonQualifiedNameTypeConstructor)
import Language.PS.CST.Types (Binder(..), Declaration(..), Expr(..), Guarded(..), Ident(..), LetBinding(..), Module(..), ProperName(..), (====>>))

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty as NonEmpty
import Prelude (($))

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ NonEmpty.cons' "MultilinePatternMatchingInLet2" []
  , imports: []
  , exports: []
  , declarations:
    [ DeclSignature
      { comments: Nothing
      , ident: Ident "myfunc"
      , type_: nonQualifiedNameTypeConstructor "Int" ====>> nonQualifiedNameTypeConstructor "Int"
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
                  , type_: nonQualifiedNameTypeConstructor "ModulePath" ====>> nonQualifiedNameTypeConstructor "Int"
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
                , LetBindingSignature
                  { ident: Ident "psModuleFile2"
                  , type_: nonQualifiedNameTypeConstructor "ModulePath" ====>> nonQualifiedNameTypeConstructor "Int"
                  }
                , LetBindingName
                  { name: Ident "psModuleFile2"
                  , binders:
                    [ BinderConstructor { name: nonQualifiedName (ProperName "Path"), args: [] }
                    ]
                  , guarded: Unconditional
                    { expr: ExprNumber (Left 1)
                    , whereBindings: []
                    }
                  }
                , LetBindingName
                  { name: Ident "psModuleFile2"
                  , binders:
                    [ BinderConstructor { name: nonQualifiedName (ProperName "Name"), args: [] }
                    ]
                  , guarded: Unconditional
                    { expr: ExprNumber (Left 2)
                    , whereBindings: []
                    }
                  }
                ]
              , body: nonQualifiedExprIdent "psModuleFile" `ExprApp` ExprNumber (Left 1)
              }
            , whereBindings: []
            }
        }
      }
    ]
  }
