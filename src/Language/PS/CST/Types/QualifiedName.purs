module Language.PS.CST.Types.QualifiedName where

import Prelude (class Eq, class Functor, class Ord, class Show, show, (<>))

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Language.PS.CST.Types.Leafs (ModuleName)

newtype QualifiedName a = QualifiedName
  { qualModule :: Maybe ModuleName
  , qualName :: a
  }
derive instance functorQualifiedName :: Functor QualifiedName
derive instance newtypeQualifiedName :: Newtype (QualifiedName a) _
derive instance eqQualifiedName :: Eq a => Eq (QualifiedName a)
derive instance ordQualifiedName :: Ord a => Ord (QualifiedName a)
instance showQualifiedName :: Show a => Show (QualifiedName a) where
  show (QualifiedName { qualModule, qualName }) = "(QualifiedName { qualModule = " <> show qualModule <> ", qualName = " <> show qualName <> " })"
