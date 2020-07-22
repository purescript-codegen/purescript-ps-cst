module Language.PS.CST.Types.QualifiedName where

import Prelude

import Data.Either (Either)
import Data.Either.Nested (type (\/))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Array.NonEmpty (NonEmptyArray)
import Language.PS.CST.Types.Shared

newtype QualifiedName a = QualifiedName
  { qualModule :: Maybe ModuleName
  , qualName :: a
  }
derive instance newtypeQualifiedName :: Newtype (QualifiedName a) _
derive instance eqQualifiedName :: Eq a => Eq (QualifiedName a)
derive instance ordQualifiedName :: Ord a => Ord (QualifiedName a)
instance showQualifiedName :: Show a => Show (QualifiedName a) where
  show (QualifiedName { qualModule, qualName }) = "(QualifiedName { qualModule = " <> show qualModule <> ", qualName = " <> show qualName <> " })"
