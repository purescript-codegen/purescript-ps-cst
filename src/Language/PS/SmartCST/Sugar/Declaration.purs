module Language.PS.SmartCST.Sugar.Declaration where

import Language.PS.SmartCST.Types.Declaration (Type(..))
import Language.PS.SmartCST.Types.SmartQualifiedName (SmartQualifiedName(..))
import Language.PS.CST.Types.Leafs (ProperName(..))
import Language.PS.CST.Sugar.Leafs (mkModuleName, mkRowLabels)
import Prelude (($))

import Data.Array.NonEmpty as NonEmpty
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))

typeRecord :: Array (String /\ Type) -> Type
typeRecord labels = TypeRecord { rowLabels: mkRowLabels labels, rowTail: Nothing }

typeRow :: Array (String /\ Type) -> Type
typeRow labels = TypeRow { rowLabels: mkRowLabels labels, rowTail: Nothing }

booleanType :: Type
booleanType = TypeConstructor $ SmartQualifiedName__Ignore $ ProperName "Boolean"

numberType :: Type
numberType = TypeConstructor $ SmartQualifiedName__Ignore $ ProperName "Number"

stringType :: Type
stringType = TypeConstructor $ SmartQualifiedName__Ignore $ ProperName "String"

arrayType :: Type -> Type
arrayType = TypeApp (TypeConstructor $ SmartQualifiedName__Ignore $ ProperName "Array")

maybeType :: Type -> Type
maybeType = TypeApp (TypeConstructor $ SmartQualifiedName__Simple (mkModuleName (NonEmpty.cons' "Data" ["Maybe"])) $ ProperName "Maybe")
