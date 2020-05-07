module Test.Golden.WithEnumWithRecord.Actual where

import Data.Either
import Data.Maybe
import Language.PS.AST
import Language.PS.AST.Types
import Language.PS.AST.Sugar
import Prelude
import Data.Tuple.Nested (type (/\), (/\))

import Data.NonEmpty ((:|))

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ "WithEnumWithRecord" :| []
  , imports: []
  , exports: []
  , declarations:
    [ DeclData
      ( DataHead
        { dataHdName: ProperName "Foo"
        , dataHdVars: []
        }
      )
      [ DataCtor
        { dataCtorName: ProperName "Bar"
        , dataCtorFields:
          [ boolean
          , typeRecordWithoutTail
            [ "foo" /\ number
            , "bar" /\ typeRecordWithoutTail [ "baz" /\ dataMapMap string number ]
            , "qwe" /\ typeRecordWithoutTail
              [ "rty" /\ (dataMapMap (typeRecordWithoutTail [ "asd" /\ number ]) (typeRecordWithoutTail [ "foo" /\ number ]))
              , "uio" /\ (dataMapMap (dataMapMap (dataMapMap number boolean) (dataMapMap number boolean)) boolean)
              ]
            ]
          , TypeVar (Ident "a")
          , array $ TypeVar (Ident "a")
          , array $ typeRecordWithoutTail [ "foo" /\ number ]
          , TypeWildcard
          , TypeHole $ Ident "myhole"
          , TypeString "PsString"
          , TypeRow $ Row { rowLabels: [], rowTail: Nothing }
          , TypeRow $ Row { rowLabels: [], rowTail: Just myExtension }
          , TypeRow $ Row { rowLabels: mkRowLabels [ "rowField" /\ number ], rowTail: Nothing }
          , TypeRow $ Row { rowLabels: mkRowLabels [ "rowField" /\ number ], rowTail: Just myExtension }
          , TypeRow $ Row { rowLabels: mkRowLabels [ "rowField" /\ number, "rowField2" /\ number ], rowTail: Nothing }
          , TypeRow $ Row { rowLabels: mkRowLabels [ "rowField" /\ number, "rowField2" /\ number ], rowTail: Just myExtension }
          , TypeRow $ Row { rowLabels: mkRowLabels [ "rowField" /\ number, "rowField2" /\ number ], rowTail: Just $ TypeOp myExtension (nonQualifiedName $ OpName "+") (nonQualifiedNameTypeConstructor "MyOtherExtension") }
          , TypeRow $ Row
            { rowLabels: mkRowLabels [ "rowField" /\ number, "rowField2" /\ number ]
            , rowTail: Just $ TypeOp myExtension
                                     (nonQualifiedName $ OpName "+")
                                     ((nonQualifiedNameTypeConstructor "MyOtherExtension")
                                      `TypeApp`
                                      (typeRecordWithoutTail [ "someField" /\ number ])
                                     )
            }
          , TypeForall
            ((typeVarName "a") :| [(TypeVarKinded (Ident "b") (KindRow (KindName $ nonQualifiedName (ProperName "Type"))) )])
            (array $ typeVar "a")
          , TypeArr (array $ typeVar "a") (maybe $ typeVar "a")
          , TypeOp (nonQualifiedNameTypeConstructor "Array") (nonQualifiedName $ OpName "~>") (nonQualifiedNameTypeConstructor "Maybe")
          , TypeForall
            ((typeVarName "f") :| [])
            ( TypeConstrained
              (Constraint { className: nonQualifiedName $ ProperName "Functor", args: [typeVar "f"] })
              (TypeOp (typeVar "f") (nonQualifiedName $ OpName "~>") (nonQualifiedNameTypeConstructor "Maybe"))
            )
          , TypeConstrained
            (Constraint { className: nonQualifiedName $ ProperName "MyClass", args: [typeVar "f", typeVar "g", typeVar "k"] })
            (TypeConstrained
              (Constraint { className: nonQualifiedName $ ProperName "MyClass2", args: [typeRecordWithoutTail $ [ "foo" /\ number ]] })
              (typeVar "f"))
          , TypeKinded
            (TypeConstructor $ nonQualifiedName $ ProperName "MyKindedType")
            ((kindNamed "CustomKind" `KindArr` KindRow (kindNamed "Type")) `KindArr` (kindNamed "Type"))
          , TypeKinded
            (TypeConstructor $ nonQualifiedName $ ProperName "MyKindedType")
            (kindNamed "CustomKind" `KindArr` (KindRow (kindNamed "Type") `KindArr` (kindNamed "Type")))
          ]
        }
      , DataCtor
        { dataCtorName: ProperName "Baz"
        , dataCtorFields:
          [ TypeConstructor $ qualifiedName (mkModuleName $ "Prelude" :| []) (ProperName "Boolean")
          ]
        }
      ]
    ]
  }
  where
    dataMapMap x y =
      (TypeConstructor $ qualifiedName (mkModuleName $ "Data" :| ["Map"]) (ProperName "Map"))
      `TypeApp`
      x
      `TypeApp`
      y

    typeRecordWithoutTail :: Array (String /\ Type) -> Type
    typeRecordWithoutTail labels =
      TypeRecord $ Row
        { rowLabels: mkRowLabels labels
        , rowTail: Nothing
        }

    mkRowLabels :: Array (String /\ Type) -> Array { label :: Label, type_ :: Type }
    mkRowLabels = map mkRowLabel

    mkRowLabel :: (String /\ Type) -> { label :: Label, type_ :: Type }
    mkRowLabel = (\(label /\ type_) -> { label: Label label, type_ })

    nonQualifiedNameTypeConstructor s = TypeConstructor $ nonQualifiedName (ProperName s)

    boolean = nonQualifiedNameTypeConstructor "Boolean"
    number = nonQualifiedNameTypeConstructor "Number"
    string = nonQualifiedNameTypeConstructor "String"
    array = TypeApp (nonQualifiedNameTypeConstructor "Array")
    maybe = TypeApp (nonQualifiedNameTypeConstructor "Maybe")

    myExtension = nonQualifiedNameTypeConstructor "MyExtension"

    -- typeForall = TypeForall (TypeVarName (Ident "a") :| [])
    typeVarName = TypeVarName <<< Ident
    typeVar = TypeVar <<< Ident

    kindNamed s = KindName (nonQualifiedName $ ProperName s)
