module Test.Golden.DeclDataComplex.Actual where

import Language.PS.CST (Constraint(..), DataCtor(..), DataHead(..), Declaration(..), Ident(..), Kind(..), Module(..), OpName(..), ProperName(..), Type(..), TypeVarBinding(..), arrayType, booleanType, maybeType, mkModuleName, mkRowLabels, nonQualifiedName, numberType, qualifiedName, stringType, typeRecord, (====>>), (====>>>))

import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty as NonEmpty
import Data.Tuple.Nested ((/\))
import Prelude

dataMapMap :: Type -> Type -> Type
dataMapMap x y =
  (TypeConstructor $ qualifiedName (mkModuleName $ NonEmpty.cons' "Data" ["Map"]) (ProperName "Map"))
  `TypeApp`
  x
  `TypeApp`
  y

myExtension :: Type
myExtension = TypeConstructor $ nonQualifiedName $ ProperName "MyExtension"

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ NonEmpty.singleton "DeclDataComplex"
  , imports: []
  , exports: []
  , declarations:
    [ DeclData
      { comments: Nothing
      , head: DataHead
        { dataHdName: ProperName "Foo"
        , dataHdVars: []
        }
      , constructors:
        [ DataCtor
          { dataCtorName: ProperName "Bar"
          , dataCtorFields:
            [ booleanType
            , typeRecord
              [ "foo" /\ numberType
              , "bar" /\ typeRecord [ "baz" /\ dataMapMap stringType numberType ]
              , "qwe" /\ typeRecord
                [ "rty" /\ (dataMapMap (typeRecord [ "asd" /\ numberType ]) (typeRecord [ "foo" /\ numberType, "bar" /\ (dataMapMap (dataMapMap (dataMapMap numberType booleanType) (dataMapMap numberType booleanType)) booleanType) ]))
                , "uio" /\ (dataMapMap (dataMapMap (dataMapMap numberType booleanType) (dataMapMap numberType booleanType)) booleanType)
                ]
              ]
            , TypeVar (Ident "a")
            , arrayType $ TypeVar (Ident "a")
            , arrayType $ typeRecord [ "foo" /\ numberType ]
            , TypeWildcard
            , TypeHole $ Ident "myhole"
            , TypeString "PsString"
            , TypeRow { rowLabels: [], rowTail: Nothing }
            , TypeRow { rowLabels: [], rowTail: Just myExtension }
            , TypeRow { rowLabels: mkRowLabels [ "rowField" /\ numberType ], rowTail: Nothing }
            , TypeRow { rowLabels: mkRowLabels [ "rowField" /\ numberType ], rowTail: Just myExtension }
            , TypeRow { rowLabels: mkRowLabels [ "rowField" /\ numberType, "rowField2" /\ numberType ], rowTail: Nothing }
            , TypeRow { rowLabels: mkRowLabels [ "rowField" /\ numberType, "rowField2" /\ numberType ], rowTail: Just myExtension }
            , TypeRow { rowLabels: mkRowLabels [ "rowField" /\ numberType, "rowField2" /\ numberType ], rowTail: Just $ TypeOp myExtension (nonQualifiedName $ OpName "+") (TypeConstructor $ nonQualifiedName $ ProperName "MyOtherExtension") }
            , TypeRow
              { rowLabels: mkRowLabels [ "rowField" /\ numberType, "rowField2" /\ numberType ]
              , rowTail: Just $ TypeOp myExtension
                                      (nonQualifiedName $ OpName "+")
                                      ((TypeConstructor $ nonQualifiedName $ ProperName "MyOtherExtension")
                                        `TypeApp`
                                        (typeRecord [ "someField" /\ numberType ])
                                      )
              }
            , TypeRow
              { rowLabels: mkRowLabels
                [ "rowField" /\ (typeRecord
                  [ "foo" /\ numberType
                  , "bar" /\ (dataMapMap (dataMapMap (dataMapMap numberType booleanType) (dataMapMap numberType booleanType)) booleanType)
                  , "baz" /\ (
                    (TypeConstructor $ nonQualifiedName (ProperName "Complex"))
                    `TypeApp`
                    (TypeConstructor $ nonQualifiedName (ProperName "A"))
                    `TypeApp`
                    (TypeConstructor $ nonQualifiedName (ProperName "B"))
                    `TypeApp`
                    (TypeConstructor $ nonQualifiedName (ProperName "C"))
                    `TypeApp`
                    (TypeConstructor $ nonQualifiedName (ProperName "D"))
                    `TypeApp`
                    (TypeConstructor $ nonQualifiedName (ProperName "E"))
                    `TypeApp`
                    (TypeConstructor $ nonQualifiedName (ProperName "F"))
                    `TypeApp`
                    (TypeConstructor $ nonQualifiedName (ProperName "G"))
                    `TypeApp`
                    (TypeConstructor $ nonQualifiedName (ProperName "H"))
                  )
                  , "qux" /\ (
                    (TypeConstructor $ nonQualifiedName (ProperName "Complex"))
                    `TypeApp`
                    (
                      (TypeConstructor $ nonQualifiedName (ProperName "A"))
                      `TypeApp`
                      (TypeConstructor $ nonQualifiedName (ProperName "B"))
                      `TypeApp`
                      (TypeConstructor $ nonQualifiedName (ProperName "C"))
                    )
                    `TypeApp`
                    (TypeConstructor $ nonQualifiedName (ProperName "D"))
                    `TypeApp`
                    (TypeConstructor $ nonQualifiedName (ProperName "E"))
                    `TypeApp`
                    (
                      (TypeConstructor $ nonQualifiedName (ProperName "F"))
                      `TypeApp`
                      (TypeConstructor $ nonQualifiedName (ProperName "G"))
                      `TypeApp`
                      (TypeConstructor $ nonQualifiedName (ProperName "H"))
                    )
                  )
                  , "asd" /\ (
                    (TypeConstructor $ nonQualifiedName (ProperName "Complex"))
                    `TypeApp`
                    (TypeConstructor $ nonQualifiedName (ProperName "A"))
                    `TypeApp`
                    (TypeConstructor $ nonQualifiedName (ProperName "B"))
                    `TypeApp`
                    (
                      (TypeConstructor $ nonQualifiedName (ProperName "C"))
                      `TypeApp`
                      (
                        (TypeConstructor $ nonQualifiedName (ProperName "D"))
                        `TypeApp`
                        (TypeConstructor $ nonQualifiedName (ProperName "E"))
                      )
                      `TypeApp`
                      (TypeConstructor $ nonQualifiedName (ProperName "F"))
                      `TypeApp`
                      (TypeConstructor $ nonQualifiedName (ProperName "G"))
                    )
                    `TypeApp`
                    (TypeConstructor $ nonQualifiedName (ProperName "H"))
                  )
                  , "qwe" /\ (
                    (TypeConstructor $ nonQualifiedName (ProperName "Complex"))
                    `TypeApp`
                    (
                      (TypeConstructor $ nonQualifiedName (ProperName "A"))
                      `TypeApp`
                      (TypeConstructor $ nonQualifiedName (ProperName "B"))
                      `TypeApp`
                      (TypeConstructor $ nonQualifiedName (ProperName "C"))
                    )
                    `TypeApp`
                    (TypeConstructor $ nonQualifiedName (ProperName "D"))
                    `TypeApp`
                    (TypeConstructor $ nonQualifiedName (ProperName "E"))
                    `TypeApp`
                    (
                      (TypeConstructor $ nonQualifiedName (ProperName "F"))
                      `TypeApp`
                      (
                        (TypeConstructor $ nonQualifiedName (ProperName "G"))
                        `TypeApp`
                        (TypeConstructor $ nonQualifiedName (ProperName "H"))
                      )
                    )
                  )
                  ])
                ]
              , rowTail: Nothing
              }
            , TypeForall
              (NonEmpty.cons' (TypeVarName $ Ident "a") [(TypeVarKinded (Ident "b") (KindRow (KindName $ nonQualifiedName (ProperName "Type"))) )])
              (arrayType $ TypeVar $ Ident "a")
            , (arrayType $ TypeVar $ Ident "a") ====>> (maybeType $ TypeVar $ Ident "a")
            , TypeOp (TypeConstructor $ nonQualifiedName $ ProperName "Array") (nonQualifiedName $ OpName "~>") (TypeConstructor $ nonQualifiedName $ ProperName "Maybe")
            , TypeForall
              (NonEmpty.singleton (TypeVarName $ Ident "f"))
              (TypeConstrained
                (Constraint { className: nonQualifiedName $ ProperName "Functor", args: [TypeVar $ Ident "f"] })
                (TypeOp (TypeVar $ Ident "f") (nonQualifiedName $ OpName "~>") (TypeConstructor $ nonQualifiedName $ ProperName "Maybe"))
              )
            , TypeConstrained
              (Constraint { className: nonQualifiedName $ ProperName "MyClass", args: [TypeVar $ Ident "f", TypeVar $ Ident "g", TypeVar $ Ident "k"] })
              (TypeConstrained
                (Constraint { className: nonQualifiedName $ ProperName "MyClass2", args: [typeRecord $ [ "foo" /\ numberType ]] })
                (TypeVar $ Ident "f"))
            , TypeKinded
              (TypeConstructor $ nonQualifiedName $ ProperName "MyKindedType")
              (((KindName $ nonQualifiedName $ ProperName "CustomKind") ====>>> KindRow (KindName $ nonQualifiedName $ ProperName "Type")) ====>>> (KindName $ nonQualifiedName $ ProperName "Type"))
            , TypeKinded
              (TypeConstructor $ nonQualifiedName $ ProperName "MyKindedType")
              ((KindName $ nonQualifiedName $ ProperName "CustomKind") ====>>> KindRow (KindName $ nonQualifiedName $ ProperName "Type") ====>>> (KindName $ nonQualifiedName $ ProperName "Type"))
            ]
          }
        , DataCtor
          { dataCtorName: ProperName "Baz"
          , dataCtorFields:
            [ TypeConstructor $ qualifiedName (mkModuleName $ NonEmpty.singleton "Prelude") (ProperName "Boolean")
            ]
          }
        ]
      }
    ]
  }
