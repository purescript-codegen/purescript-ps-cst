module Test.Golden.Html.Actual where

import Language.PS.CST (Declaration(..), Expr(..), Guarded(..), Ident(..), Module(..), ProperName(..), Type(..), mkModuleName, nonQualifiedName)
import Prelude

import Data.Array.NonEmpty as NonEmpty
import Data.Maybe (Maybe(..))

declValue :: String -> Type -> Expr -> Array Declaration
declValue name type_ expr =
  [ DeclSignature
    { comments: Nothing
    , ident: Ident name
    , type_
    }
  , DeclValue
    { comments: Nothing
    , valueBindingFields:
      { name: Ident name
      , binders: []
      , guarded: Unconditional
          { expr
          , whereBindings: []
          }
      }
    }
  ]

exprIdent :: String -> Expr
exprIdent n = ExprIdent (nonQualifiedName (Ident n))

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ NonEmpty.singleton "Array"
  , imports: []
  , exports: []
  , declarations:
    ( declValue
      "html"
      ( TypeConstructor $ nonQualifiedName (ProperName "HTML") )
      ( exprIdent "div"
        `ExprApp`
        ExprArray
        [ exprIdent "class_" `ExprApp` exprIdent "main"
        , exprIdent "id" `ExprApp` ExprString "zero"
        ]
        `ExprApp`
        ExprArray
        [ exprIdent "div"
          `ExprApp`
          ExprArray
          [ exprIdent "class_" `ExprApp` exprIdent "main"
          , exprIdent "id" `ExprApp` ExprString "zero"
          ]
          `ExprApp`
          ExprArray
          [ exprIdent "div"
            `ExprApp`
            ExprArray
            [ exprIdent "class_" `ExprApp` exprIdent "main"
            , exprIdent "id" `ExprApp` ExprString "zero"
            ]
            `ExprApp`
            ExprArray
            [
            ]
          , exprIdent "div"
            `ExprApp`
            ExprArray
            [ exprIdent "class_" `ExprApp` exprIdent "main"
            , exprIdent "id" `ExprApp` ExprString "zero"
            ]
            `ExprApp`
            ExprArray
            [
            ]
          ]
        , exprIdent "div"
          `ExprApp`
          ExprArray
          [ exprIdent "class_" `ExprApp` exprIdent "main"
          , exprIdent "id" `ExprApp` ExprString "zero"
          ]
          `ExprApp`
          ExprArray
          [ exprIdent "div"
            `ExprApp`
            ExprArray
            [ exprIdent "class_" `ExprApp` exprIdent "main"
            , exprIdent "id" `ExprApp` ExprString "zero"
            ]
            `ExprApp`
            ExprArray
            [
            ]
          , exprIdent "div"
            `ExprApp`
            ExprArray
            [ exprIdent "class_" `ExprApp` exprIdent "main"
            , exprIdent "id" `ExprApp` ExprString "zero"
            ]
            `ExprApp`
            ExprArray
            [
            ]
          ]
        ]
      )
    )
  }
