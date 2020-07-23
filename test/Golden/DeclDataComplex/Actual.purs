module Test.Golden.DeclDataComplex.Actual where

import Language.PS.CST

import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty as NonEmpty
import Data.Tuple.Nested ((/\))
import Prelude (($))

dataMapMap :: Type -> Type -> Type
dataMapMap x y =
  (TypeConstructor $ qualifiedName (mkModuleName $ NonEmpty.cons' "Data" ["Map"]) (ProperName "Map"))
  `TypeApp`
  x
  `TypeApp`
  y

myExtension :: Type
myExtension = nonQualifiedNameTypeConstructor "MyExtension"

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
            , TypeRow { rowLabels: mkRowLabels [ "rowField" /\ numberType, "rowField2" /\ numberType ], rowTail: Just $ TypeOp myExtension (nonQualifiedName $ OpName "+") (nonQualifiedNameTypeConstructor "MyOtherExtension") }
            , TypeRow
              { rowLabels: mkRowLabels [ "rowField" /\ numberType, "rowField2" /\ numberType ]
              , rowTail: Just $ TypeOp myExtension
                                      (nonQualifiedName $ OpName "+")
                                      ((nonQualifiedNameTypeConstructor "MyOtherExtension")
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
              (NonEmpty.cons' (typeVarName "a") [(TypeVarKinded (Ident "b") (KindRow (KindName $ nonQualifiedName (ProperName "Type"))) )])
              (arrayType $ typeVar "a")
            , (arrayType $ typeVar "a") ====>> (maybeType $ typeVar "a")
            , TypeOp (nonQualifiedNameTypeConstructor "Array") (nonQualifiedName $ OpName "~>") (nonQualifiedNameTypeConstructor "Maybe")
            , TypeForall
              (NonEmpty.singleton (typeVarName "f"))
              (TypeConstrained
                (Constraint { className: nonQualifiedName $ ProperName "Functor", args: [typeVar "f"] })
                (TypeOp (typeVar "f") (nonQualifiedName $ OpName "~>") (nonQualifiedNameTypeConstructor "Maybe"))
              )
            , TypeConstrained
              (Constraint { className: nonQualifiedName $ ProperName "MyClass", args: [typeVar "f", typeVar "g", typeVar "k"] })
              (TypeConstrained
                (Constraint { className: nonQualifiedName $ ProperName "MyClass2", args: [typeRecord $ [ "foo" /\ numberType ]] })
                (typeVar "f"))
            , TypeKinded
              (TypeConstructor $ nonQualifiedName $ ProperName "MyKindedType")
              ((kindNamed "CustomKind" ====>>> KindRow (kindNamed "Type")) ====>>> (kindNamed "Type"))
            , TypeKinded
              (TypeConstructor $ nonQualifiedName $ ProperName "MyKindedType")
              (kindNamed "CustomKind" ====>>> KindRow (kindNamed "Type") ====>>> (kindNamed "Type"))
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
