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

-- TOOD: remove
nonQualifiedNameTypeConstructor :: String -> Type
nonQualifiedNameTypeConstructor s = TypeConstructor $ nonQualifiedName $ ProperName s

-- TOOD: remove
nonQualifiedNameExprConstructor :: String -> Expr
nonQualifiedNameExprConstructor s = ExprConstructor $ nonQualifiedName $ ProperName s

booleanType :: Type
booleanType = nonQualifiedNameTypeConstructor "Boolean"

numberType :: Type
numberType = nonQualifiedNameTypeConstructor "Number"

stringType :: Type
stringType = nonQualifiedNameTypeConstructor "String"

arrayType :: Type -> Type
arrayType = TypeApp (nonQualifiedNameTypeConstructor "Array")

maybeType :: Type -> Type
maybeType = TypeApp (nonQualifiedNameTypeConstructor "Maybe")

-- TODO: remove
typeVarName :: String -> TypeVarBinding
typeVarName = TypeVarName <<< Ident -- for left side of forall

typeVar :: String -> Type
typeVar = TypeVar <<< Ident -- for right side of forall and constructor arguments

kindNamed :: String -> Kind
kindNamed s = KindName (nonQualifiedName $ ProperName s)

nonQualifiedExprIdent :: String -> Expr
nonQualifiedExprIdent s = ExprIdent $ nonQualifiedName (Ident s)

emptyRow :: Row
emptyRow = { rowLabels: [], rowTail: Nothing }

