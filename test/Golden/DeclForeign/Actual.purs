module Test.Golden.DeclForeign.Actual where

import Language.PS.CST.Sugar (mkModuleName, mkRowLabels, nonQualifiedName, nonQualifiedNameTypeConstructor, typeVar, typeVarName)
import Language.PS.CST.Types (Declaration(..), Foreign(..), Ident(..), Kind(..), Module(..), ProperName(..), Row(..), Type(..), (====>>>))
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
            (TypeRow $ Row { rowLabels: mkRowLabels [ "console" /\ nonQualifiedNameTypeConstructor "CONSOLE", "foo" /\ nonQualifiedNameTypeConstructor "FOO" ], rowTail: Just $ typeVar "e" })
            `TypeApp`
            nonQualifiedNameTypeConstructor "Unit"
          )
        }
      }
    ]
  }
