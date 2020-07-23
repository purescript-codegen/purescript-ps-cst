module Language.PS.CST.Sugar.QualifiedName where

import Language.PS.CST.Types.QualifiedName (QualifiedName(..))
import Language.PS.CST.Types.Leafs (ModuleName)

import Data.Maybe (Maybe(..))

nonQualifiedName :: ∀ a . a -> QualifiedName a
nonQualifiedName a = QualifiedName { qualModule: Nothing, qualName: a }

qualifiedName :: ∀ a . ModuleName -> a -> QualifiedName a
qualifiedName moduleName a = QualifiedName { qualModule: Just moduleName, qualName: a }

