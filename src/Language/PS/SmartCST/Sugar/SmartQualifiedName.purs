module Language.PS.SmartCST.Sugar.SmartQualifiedName where

import Language.PS.CST.Sugar.Leafs
import Language.PS.CST.Types.Leafs
import Language.PS.SmartCST.Types.SmartQualifiedName

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (singleton) as NonEmpty
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Prelude (map, ($), (<<<))

smartQualifiedNameNone :: ∀ a . ModuleName -> a -> SmartQualifiedName a
smartQualifiedNameNone module_ name = SmartQualifiedName { module_, importType: SmartQualifiedNameImportType__None, name }

smartQualifiedNameFull :: ∀ a . ModuleName -> a -> SmartQualifiedName a
smartQualifiedNameFull module_ name = SmartQualifiedName { module_, importType: SmartQualifiedNameImportType__Full, name }

smartQualifiedNameCustom :: ∀ a . ModuleName -> ModuleName -> a -> SmartQualifiedName a
smartQualifiedNameCustom module_ customModule name = SmartQualifiedName { module_, importType: SmartQualifiedNameImportType__Custom customModule, name }

