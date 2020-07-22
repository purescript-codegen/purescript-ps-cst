module Language.PS.CST.Sugar where

import Language.PS.CST.Types.Shared (Expr(..), Ident(..), Kind(..), Label(..), ModuleName(..), ProperName(..), Row(..), Type(..), TypeVarBinding(..))
import Language.PS.CST.Types.QualifiedName
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
typeRecord :: Array (String /\ Type QualifiedName) -> Type QualifiedName
typeRecord labels =
  TypeRecord $ Row
    { rowLabels: mkRowLabels labels
    , rowTail: Nothing
    }

-- TOOD: remove
typeRow :: Array (String /\ Type QualifiedName) -> Type QualifiedName
typeRow labels =
  TypeRow $ Row
    { rowLabels: mkRowLabels labels
    , rowTail: Nothing
    }

mkRowLabels :: Array (String /\ Type QualifiedName) -> Array { label :: Label, type_ :: Type QualifiedName }
mkRowLabels = map mkRowLabel

mkRowLabel :: (String /\ Type QualifiedName) -> { label :: Label, type_ :: Type QualifiedName }
mkRowLabel = (\(label /\ type_) -> { label: Label label, type_ })

-- TOOD: remove
nonQualifiedNameTypeConstructor :: String -> Type QualifiedName
nonQualifiedNameTypeConstructor s = TypeConstructor $ nonQualifiedName $ ProperName s

-- TOOD: remove
nonQualifiedNameExprConstructor :: String -> Expr QualifiedName
nonQualifiedNameExprConstructor s = ExprConstructor $ nonQualifiedName $ ProperName s

booleanType :: Type QualifiedName
booleanType = nonQualifiedNameTypeConstructor "Boolean"

numberType :: Type QualifiedName
numberType = nonQualifiedNameTypeConstructor "Number"

stringType :: Type QualifiedName
stringType = nonQualifiedNameTypeConstructor "String"

arrayType :: Type QualifiedName -> Type QualifiedName
arrayType = TypeApp (nonQualifiedNameTypeConstructor "Array")

maybeType :: Type QualifiedName -> Type QualifiedName
maybeType = TypeApp (nonQualifiedNameTypeConstructor "Maybe")

-- TODO: remove
typeVarName :: String -> TypeVarBinding QualifiedName
typeVarName = TypeVarName <<< Ident -- for left side of forall

typeVar :: String -> Type QualifiedName
typeVar = TypeVar <<< Ident -- for right side of forall and constructor arguments

kindNamed :: String -> Kind QualifiedName
kindNamed s = KindName (nonQualifiedName $ ProperName s)

nonQualifiedExprIdent :: String -> Expr QualifiedName
nonQualifiedExprIdent s = ExprIdent $ nonQualifiedName (Ident s)

-- emptyRow :: Row
-- emptyRow = Row { labels: mempty, tail: Nothing }

