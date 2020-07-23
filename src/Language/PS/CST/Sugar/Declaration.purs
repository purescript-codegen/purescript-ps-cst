module Language.PS.CST.Sugar.Declaration where

import Language.PS.CST.Types.Declaration
import Language.PS.CST.Types.QualifiedName
import Language.PS.CST.Types.Leafs
import Language.PS.CST.Sugar.QualifiedName
import Language.PS.CST.Sugar.Leafs
import Prelude (map, ($), (<<<))

import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Tuple.Nested (type (/\))

typeRecord :: Array (String /\ Type) -> Type
typeRecord labels = TypeRecord { rowLabels: mkRowLabels labels, rowTail: Nothing }

typeRow :: Array (String /\ Type) -> Type
typeRow labels = TypeRow { rowLabels: mkRowLabels labels, rowTail: Nothing }

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
