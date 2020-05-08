module Test.Golden.WithEnumWithRecord.Actual where

import Data.Maybe (Maybe(..))
import Language.PS.AST.Types
import Language.PS.AST.Sugar
import Prelude (map, ($), (<<<))
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
          , typeRecord
            [ "foo" /\ number
            , "bar" /\ typeRecord [ "baz" /\ dataMapMap string number ]
            , "qwe" /\ typeRecord
              [ "rty" /\ (dataMapMap (typeRecord [ "asd" /\ number ]) (typeRecord [ "foo" /\ number, "bar" /\ (dataMapMap (dataMapMap (dataMapMap number boolean) (dataMapMap number boolean)) boolean) ]))
              , "uio" /\ (dataMapMap (dataMapMap (dataMapMap number boolean) (dataMapMap number boolean)) boolean)
              ]
            ]
          , TypeVar (Ident "a")
          , array $ TypeVar (Ident "a")
          , array $ typeRecord [ "foo" /\ number ]
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
                                      (typeRecord [ "someField" /\ number ])
                                     )
            }
          , TypeRow $ Row { rowLabels: mkRowLabels [ "rowField" /\ (typeRecord [ "foo" /\ number, "bar" /\ (dataMapMap (dataMapMap (dataMapMap number boolean) (dataMapMap number boolean)) boolean) ]) ], rowTail: Nothing }
          , TypeForall
            ((typeVarName "a") :| [(TypeVarKinded (Ident "b") (KindRow (KindName $ nonQualifiedName (ProperName "Type"))) )])
            (array $ typeVar "a")
          , (array $ typeVar "a") ====>> (maybe $ typeVar "a")
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
              (Constraint { className: nonQualifiedName $ ProperName "MyClass2", args: [typeRecord $ [ "foo" /\ number ]] })
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

    myExtension = nonQualifiedNameTypeConstructor "MyExtension"
