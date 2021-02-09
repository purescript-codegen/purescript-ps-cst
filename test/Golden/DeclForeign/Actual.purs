module Test.Golden.DeclForeign.Actual where

import Language.PS.CST (Declaration(..), Foreign(..), Ident(..), Kind(..), Module(..), ProperName(..), PSType(..), TypeVarBinding(..), mkModuleName, mkRowLabels, nonQualifiedName, (====>>>))
import Prelude

import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty as NonEmpty
import Data.Tuple.Nested ((/\))

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ NonEmpty.cons' "DeclForeign" []
  , imports: []
  , exports: []
  , declarations:
    [ DeclForeign
      { comments: Nothing
      , foreign_: ForeignKind { name: ProperName "Foo" }
      }
    , DeclForeign
      { comments: Nothing
      , foreign_: ForeignData { name: ProperName "Foo", kind_: KindRow (KindName $ nonQualifiedName (ProperName "PSType")) ====>>> (KindName $ nonQualifiedName (ProperName "PSType")) }
      }
    , DeclForeign
      { comments: Nothing
      , foreign_: ForeignValue
        { ident: Ident "main_"
        , type_: TypeForall (NonEmpty.cons' (TypeVarName $ Ident "e") []) (
            (TypeConstructor $ nonQualifiedName $ ProperName "Eff")
            `TypeApp`
            ( TypeRow
              { rowLabels: mkRowLabels
                [ "console" /\ (TypeConstructor $ nonQualifiedName $ ProperName "CONSOLE")
                , "foo" /\ (TypeConstructor $ nonQualifiedName $ ProperName "FOO")
                , "bar" /\ (TypeConstructor $ nonQualifiedName $ ProperName "BAR")
                , "baz" /\ (TypeConstructor $ nonQualifiedName $ ProperName "BAZ")
                , "qux" /\ (TypeConstructor $ nonQualifiedName $ ProperName "QUX")
                , "quux" /\ (TypeConstructor $ nonQualifiedName $ ProperName "QUUX")
                ]
              , rowTail: Just $ TypeVar $ Ident "e"
              }
            )
            `TypeApp`
            (TypeConstructor $ nonQualifiedName $ ProperName "Unit")
          )
        }
      }
    ]
  }
