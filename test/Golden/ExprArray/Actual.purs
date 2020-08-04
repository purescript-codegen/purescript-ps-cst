module Test.Golden.ExprArray.Actual where

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

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ NonEmpty.singleton "Array"
  , imports: []
  , exports: []
  , declarations:
    ( declValue
      "f"
      ( (TypeConstructor $ nonQualifiedName (ProperName "Array"))
        `TypeApp`
        (TypeRecord
          { rowLabels:
            mkRowLabels
            [ "x" /\ TypeConstructor (nonQualifiedName (ProperName "Int"))
            , "y" /\ TypeConstructor (nonQualifiedName (ProperName "Boolean"))
            ]
          , rowTail: Nothing
          }
        )
      )
      ( ExprArray
        [ ExprRecord [ RecordField (Label "x") (ExprNumber (Left 1)), RecordField (Label "y") (ExprBoolean true) ]
        , ExprRecord [ RecordField (Label "x") (ExprNumber (Left 10)), RecordField (Label "y") (ExprBoolean false) ]
        ]
      )
    ) <>
    ( declValue
      "f"
      ( (TypeConstructor $ nonQualifiedName (ProperName "Array"))
        `TypeApp`
        (TypeRecord
          { rowLabels:
            mkRowLabels
            [ "x" /\ TypeConstructor (nonQualifiedName (ProperName "Int"))
            , "y" /\ TypeConstructor (nonQualifiedName (ProperName "Boolean"))
            ]
          , rowTail: Nothing
          }
        )
      )
      ( ExprArray
        [ ExprRecord [ RecordField (Label "x") (ExprNumber (Left 1)), RecordField (Label "y") (ExprBoolean true) ]
        , ExprRecord [ RecordField (Label "x") (ExprNumber (Left 10)), RecordField (Label "y") (ExprBoolean false) ]
        , ExprRecord [ RecordField (Label "x") (ExprNumber (Left 0)), RecordField (Label "y") (ExprBoolean true) ]
        , ExprRecord [ RecordField (Label "x") (ExprNumber (Left 1)), RecordField (Label "y") (ExprBoolean true) ]
        , ExprRecord [ RecordField (Label "x") (ExprNumber (Left 1)), RecordField (Label "y") (ExprBoolean true) ]
        , ExprRecord [ RecordField (Label "x") (ExprNumber (Left 1)), RecordField (Label "y") (ExprBoolean true) ]
        , ExprRecord [ RecordField (Label "x") (ExprNumber (Left 1)), RecordField (Label "y") (ExprBoolean true) ]
        , ExprRecord [ RecordField (Label "x") (ExprNumber (Left 1)), RecordField (Label "y") (ExprBoolean true) ]
        ]
      )
    )
  }
