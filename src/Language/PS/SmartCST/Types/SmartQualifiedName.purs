module Language.PS.SmartCST.Types.SmartQualifiedName where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Language.PS.CST.Types.Leafs (ModuleName)

data SmartQualifiedName a
  -- not imported at all (used for variables from Prim module or current module)
  -- used in code as `foo`
  = SmartQualifiedName__Ignore
    a
  -- imported as `import Module.Name (foo) as Module.Name`
  -- used in code as `Module.Name.foo`
  | SmartQualifiedName__Full
    ModuleName -- original
    a
  -- imported as `import Module.Name (foo)`
  -- used in code as `foo`
  | SmartQualifiedName__Simple
    ModuleName -- original
    a
  -- imported as `import Module.Name (foo) as Custom.Custom`
  -- used in code as `Custom.Custom.foo`
  | SmartQualifiedName__Custom
    ModuleName -- original
    ModuleName -- custom
    a -- name

derive instance genericSmartQualifiedName :: Generic (SmartQualifiedName a) _
derive instance functorSmartQualifiedName :: Functor SmartQualifiedName
derive instance eqQualifiedName :: Eq a => Eq (SmartQualifiedName a)
derive instance ordQualifiedName :: Ord a => Ord (SmartQualifiedName a)
instance showQualifiedName :: Show a => Show (SmartQualifiedName a) where show = genericShow
