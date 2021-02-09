module Language.PS.SmartCST.Sugar.Declaration where

import Language.PS.SmartCST.Types.Declaration (PSType(..))
import Language.PS.SmartCST.Types.SmartQualifiedName (SmartQualifiedName(..))
import Language.PS.CST.Types.Leafs (ProperName(..))
import Language.PS.CST.Sugar.Leafs (mkModuleName, mkRowLabels)
import Prelude

import Data.Array.NonEmpty as NonEmpty
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))

typeRecord :: Array (String /\ PSType) -> PSType
typeRecord labels = TypeRecord { rowLabels: mkRowLabels labels, rowTail: Nothing }

typeRow :: Array (String /\ PSType) -> PSType
typeRow labels = TypeRow { rowLabels: mkRowLabels labels, rowTail: Nothing }

booleanType :: PSType
booleanType = TypeConstructor $ SmartQualifiedName__Ignore $ ProperName "Boolean"

numberType :: PSType
numberType = TypeConstructor $ SmartQualifiedName__Ignore $ ProperName "Number"

stringType :: PSType
stringType = TypeConstructor $ SmartQualifiedName__Ignore $ ProperName "String"

arrayType :: PSType -> PSType
arrayType = TypeApp (TypeConstructor $ SmartQualifiedName__Ignore $ ProperName "Array")

maybeType :: PSType -> PSType
maybeType = TypeApp (TypeConstructor $ SmartQualifiedName__Simple (mkModuleName (NonEmpty.cons' "Data" ["Maybe"])) $ ProperName "Maybe")
