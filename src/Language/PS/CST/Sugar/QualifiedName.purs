module Language.PS.CST.Sugar.QualifiedName where

import Language.PS.CST.Types.QualifiedName
import Language.PS.CST.Types.Leafs
import Prelude (map, ($), (<<<))

import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Tuple.Nested (type (/\), (/\))

nonQualifiedName :: ∀ a . a -> QualifiedName a
nonQualifiedName a = QualifiedName { qualModule: Nothing, qualName: a }

qualifiedName :: ∀ a . ModuleName -> a -> QualifiedName a
qualifiedName moduleName a = QualifiedName { qualModule: Just moduleName, qualName: a }

