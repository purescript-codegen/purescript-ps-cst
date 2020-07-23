module Test.Golden.DeclForeign.Actual where

import Language.PS.CST
import Prelude (($))

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
      , foreign_: ForeignData { name: ProperName "Foo", kind_: KindRow (KindName $ nonQualifiedName (ProperName "Type")) ====>>> (KindName $ nonQualifiedName (ProperName "Type")) }
      }
    , DeclForeign
      { comments: Nothing
      , foreign_: ForeignValue
        { ident: Ident "main_"
        , type_: TypeForall (NonEmpty.cons' (typeVarName "e") []) (
            (TypeConstructor $ nonQualifiedName $ ProperName "Eff")
            `TypeApp`
            ( TypeRow
              { rowLabels: mkRowLabels
                [ "console" /\ (TypeConstructor $ nonQualifiedName $ ProperName "CONSOLE")
                , "foo" /\ (TypeConstructor $ nonQualifiedName $ ProperName "FOO")
                ]
              , rowTail: Just $ typeVar "e"
              }
            )
            `TypeApp`
            (TypeConstructor $ nonQualifiedName $ ProperName "Unit")
          )
        }
      }
    ]
  }
