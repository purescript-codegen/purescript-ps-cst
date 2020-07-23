module Language.PS.CST.Sugar where

import Language.PS.CST.Types.Declaration
import Language.PS.CST.Types.QualifiedName
import Language.PS.CST.Types.Leafs
import Prelude (map, ($), (<<<))

import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Tuple.Nested (type (/\), (/\))

mkModuleName :: NonEmptyArray String -> ModuleName
mkModuleName = ModuleName <<< map ProperName

nonQualifiedName :: ∀ a . a -> QualifiedName a
nonQualifiedName a = QualifiedName { qualModule: Nothing, qualName: a }

qualifiedName :: ∀ a . ModuleName -> a -> QualifiedName a
qualifiedName moduleName a = QualifiedName { qualModule: Just moduleName, qualName: a }

-- TOOD: remove
typeRecord :: Array (String /\ Type) -> Type
typeRecord labels =
  TypeRecord
    { rowLabels: mkRowLabels labels
    , rowTail: Nothing
    }

-- TOOD: remove
typeRow :: Array (String /\ Type) -> Type
typeRow labels =
  TypeRow
    { rowLabels: mkRowLabels labels
    , rowTail: Nothing
    }

mkRowLabels :: Array (String /\ Type) -> Array { label :: Label, type_ :: Type }
mkRowLabels = map mkRowLabel

mkRowLabel :: (String /\ Type) -> { label :: Label, type_ :: Type }
mkRowLabel = (\(label /\ type_) -> { label: Label label, type_ })

booleanType :: Type
booleanType = TypeConstructor $ nonQualifiedName $ ProperName "Boolean"

numberType :: Type
numberType = TypeConstructor $ nonQualifiedName $ ProperName "Number"

stringType :: Type
stringType = TypeConstructor $ nonQualifiedName $ ProperName "String"

arrayType :: Type -> Type
arrayType = TypeApp (TypeConstructor $ nonQualifiedName $ ProperName "Array")

maybeType :: Type -> Type
maybeType = TypeApp (TypeConstructor $ nonQualifiedName $ ProperName "Maybe")

typeVar :: String -> Type
typeVar = TypeVar <<< Ident -- for right side of forall and constructor arguments

kindNamed :: String -> Kind
kindNamed s = KindName (nonQualifiedName $ ProperName s)

nonQualifiedExprIdent :: String -> Expr
nonQualifiedExprIdent s = ExprIdent $ nonQualifiedName (Ident s)

emptyRow :: Row
emptyRow = { rowLabels: [], rowTail: Nothing }

