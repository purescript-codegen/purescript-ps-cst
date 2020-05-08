module Test.Golden.DeclForeign.Actual where

import Language.PS.AST.Sugar
import Language.PS.AST.Types
import Prelude

import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Tuple.Nested (type (/\), (/\))

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ "DeclForeign" :| []
  , imports: []
  , exports: []
  , declarations:
    [ DeclForeign $ ForeignKind { name: ProperName "Foo" }
    , DeclForeign $ ForeignData { name: ProperName "Foo", kind_: KindRow (KindName $ nonQualifiedName (ProperName "Type")) ====>>> (KindName $ nonQualifiedName (ProperName "Type")) }
    , DeclForeign $ ForeignValue
      { ident: Ident "main_"
      , type_: TypeForall ((typeVarName "e") :| []) (
          (TypeConstructor $ nonQualifiedName $ ProperName "Eff")
          `TypeApp`
          (TypeRow $ Row { rowLabels: mkRowLabels [ "console" /\ nonQualifiedNameTypeConstructor "CONSOLE", "foo" /\ nonQualifiedNameTypeConstructor "FOO" ], rowTail: Just $ typeVar "e" })
          `TypeApp`
          nonQualifiedNameTypeConstructor "Unit"
        )
      }
    ]
  }
