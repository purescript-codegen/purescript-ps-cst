module Language.PS.SmartCST.Types.SmartQualifiedName where

import Prelude

import Data.Either (Either)
import Data.Either.Nested (type (\/))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Array.NonEmpty (NonEmptyArray)
import Language.PS.CST.Types.Leafs

data SmartQualifiedNameImportType
  -- imported as `import Module.Name (foo) as Module.Name`
  -- used in code as `Module.Name.foo`
  = SmartQualifiedNameImportType__Full
  -- imported as `import Module.Name (foo)`
  -- used in code as `foo`
  | SmartQualifiedNameImportType__None
  -- imported as `import Module.Name (foo) as Custom.Custom`
  -- used in code as `Custom.Custom.foo`
  | SmartQualifiedNameImportType__Custom ModuleName

newtype SmartQualifiedName a = SmartQualifiedName
  { module_ :: ModuleName
  , importType :: SmartQualifiedNameImportType
  , name :: a
  }
derive instance functorSmartQualifiedName :: Functor SmartQualifiedName
-- | derive instance newtypeQualifiedName :: Newtype (SmartQualifiedName a) _
-- | derive instance eqQualifiedName :: Eq a => Eq (SmartQualifiedName a)
-- | derive instance ordQualifiedName :: Ord a => Ord (SmartQualifiedName a)
-- | instance showQualifiedName :: Show a => Show (SmartQualifiedName a) where
-- |   show (SmartQualifiedName { qualModule, qualName }) = "(SmartQualifiedName { qualModule = " <> show qualModule <> ", qualName = " <> show qualName <> " })"
