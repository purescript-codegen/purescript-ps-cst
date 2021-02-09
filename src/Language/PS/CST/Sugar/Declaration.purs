module Language.PS.CST.Sugar.Declaration where

import Language.PS.CST.Types.Declaration (PSType(..))
import Language.PS.CST.Types.Leafs (ProperName(..))
import Language.PS.CST.Sugar.QualifiedName (nonQualifiedName)
import Language.PS.CST.Sugar.Leafs (mkRowLabels)
import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))

typeRecord :: Array (String /\ PSType) -> PSType
typeRecord labels = TypeRecord { rowLabels: mkRowLabels labels, rowTail: Nothing }

typeRow :: Array (String /\ PSType) -> PSType
typeRow labels = TypeRow { rowLabels: mkRowLabels labels, rowTail: Nothing }

booleanType :: PSType
booleanType = TypeConstructor $ nonQualifiedName $ ProperName "Boolean"

numberType :: PSType
numberType = TypeConstructor $ nonQualifiedName $ ProperName "Number"

stringType :: PSType
stringType = TypeConstructor $ nonQualifiedName $ ProperName "String"

arrayType :: PSType -> PSType
arrayType = TypeApp (TypeConstructor $ nonQualifiedName $ ProperName "Array")

maybeType :: PSType -> PSType
maybeType = TypeApp (TypeConstructor $ nonQualifiedName $ ProperName "Maybe")
