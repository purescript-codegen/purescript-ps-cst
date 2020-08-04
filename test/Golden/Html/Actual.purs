module Test.Golden.Html.Actual where

import Language.PS.CST
import Prelude

import Data.Array.NonEmpty as NonEmpty
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))

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
