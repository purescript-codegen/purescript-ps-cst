module Language.PS.SmartCST.Sugar.Declaration where

import Language.PS.SmartCST.Types.Declaration
import Language.PS.SmartCST.Types.SmartQualifiedName
import Language.PS.SmartCST.Sugar.SmartQualifiedName
import Language.PS.CST.Types.Leafs
import Language.PS.CST.Sugar.Leafs
import Prelude (map, ($), (<<<))

import Data.Array.NonEmpty as NonEmpty
import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Tuple.Nested (type (/\))

typeRecord :: Array (String /\ Type) -> Type
typeRecord labels = TypeRecord { rowLabels: mkRowLabels labels, rowTail: Nothing }

typeRow :: Array (String /\ Type) -> Type
typeRow labels = TypeRow { rowLabels: mkRowLabels labels, rowTail: Nothing }

booleanType :: Type
booleanType = TypeConstructor $ smartQualifiedNameNone (mkModuleName (NonEmpty.singleton "Prim")) $ ProperName "Boolean"

numberType :: Type
numberType = TypeConstructor $ smartQualifiedNameNone (mkModuleName (NonEmpty.singleton "Prim")) $ ProperName "Number"

stringType :: Type
stringType = TypeConstructor $ smartQualifiedNameNone (mkModuleName (NonEmpty.singleton "Prim")) $ ProperName "String"

arrayType :: Type -> Type
arrayType = TypeApp (TypeConstructor $ smartQualifiedNameNone (mkModuleName (NonEmpty.singleton "Prim")) $ ProperName "Array")

maybeType :: Type -> Type
maybeType = TypeApp (TypeConstructor $ smartQualifiedNameNone (mkModuleName (NonEmpty.cons' "Data" ["Maybe"])) $ ProperName "Maybe")
