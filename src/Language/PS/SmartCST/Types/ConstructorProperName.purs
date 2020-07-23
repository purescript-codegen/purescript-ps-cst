module Language.PS.SmartCST.Types.ConstructorProperName where


import Language.PS.CST.Types.Leafs (ProperName, ProperNameType_ConstructorName, ProperNameType_TypeName)

-- | it's like `ProperName ProperNameType_ConstructorName`
-- | but also contains information of parent type `ProperName ProperNameType_TypeName`
-- |
-- | We can use it inside of `SmartQualifiedName` and:
-- | 1. build imports `import Module.Name (FooType(..))` - we need `type_` for that, but not `constructor`
-- | 2. build Expr `FooConstr` - we need `constructor` for that, but not `type_`
newtype ConstructorProperName = ConstructorProperName
  { constructor :: ProperName ProperNameType_ConstructorName
  , type_ :: ProperName ProperNameType_TypeName
  }
