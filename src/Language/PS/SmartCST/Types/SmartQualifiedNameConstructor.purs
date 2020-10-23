module Language.PS.SmartCST.Types.SmartQualifiedNameConstructor where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Language.PS.CST.Types.Leafs (ProperName, ProperNameType_ConstructorName, ProperNameType_TypeName, ModuleName)

-- used to replace `QualifiedName (ProperName ProperNameType_ClassName)`
--
-- because we need `ProperName ProperNameType_TypeName` everywhere except `SmartQualifiedNameConstructor__Ignore`
-- in order to generate `import Module.Name (FooType(..))`
data SmartQualifiedNameConstructor
  -- not imported at all
  -- used in code as `FooConst`
  = SmartQualifiedNameConstructor__Ignore
    (ProperName ProperNameType_ConstructorName)
  -- imported as `import Module.Name (FooType(..)) as Module.Name`
  -- used in code as `Module.Name.FooConst`
  | SmartQualifiedNameConstructor__Full
    ModuleName -- original
    (ProperName ProperNameType_ConstructorName)
    (ProperName ProperNameType_TypeName)
  -- imported as `import Module.Name (FooType(..))`
  -- used in code as `FooConst`
  | SmartQualifiedNameConstructor__Simple
    ModuleName -- original
    (ProperName ProperNameType_ConstructorName)
    (ProperName ProperNameType_TypeName)
  -- imported as `import Module.Name (FooType(..)) as Custom.Custom`
  -- used in code as `Custom.Custom.FooConst`
  | SmartQualifiedNameConstructor__Custom
    ModuleName -- original
    ModuleName -- custom
    (ProperName ProperNameType_ConstructorName) -- name
    (ProperName ProperNameType_TypeName)


derive instance genericSmartQualifiedNameConstructor :: Generic SmartQualifiedNameConstructor _
derive instance eqQualifiedNameConstructor :: Eq SmartQualifiedNameConstructor
derive instance ordQualifiedNameConstructor :: Ord SmartQualifiedNameConstructor
instance showQualifiedNameConstructor :: Show SmartQualifiedNameConstructor where show = genericShow
