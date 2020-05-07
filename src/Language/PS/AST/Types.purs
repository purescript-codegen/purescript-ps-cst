module Language.PS.AST.Types where

-- | This module is somewhat inspired by `purescript-cst` types.
-- | I've tried to preserve constructor names to simplify
-- | further "copy and paste based development".

import Prelude

import Data.Either (Either)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Functor.Mu (Mu)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Map (unionWith) as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.NonEmpty (NonEmpty(..))
import Data.Set (Set)
import Data.Set (fromFoldable, union) as Set
import Data.Traversable (class Traversable, sequence, traverseDefault)

-- | No need for imports list as they are collected from declarations
-- | during final codegen.
newtype Module = Module
  { moduleName :: ModuleName
  , imports :: Array ImportDecl
  , exports :: Array Export
  , declarations :: Array Declaration
  }

newtype ModuleName = ModuleName (NonEmpty Array (ProperName ProperNameType_Namespace))
derive instance newtypeModuleName :: Newtype ModuleName _
derive instance genericModuleName :: Generic ModuleName _
derive instance eqModuleName :: Eq ModuleName
derive instance ordModuleName :: Ord ModuleName
instance showModuleName :: Show ModuleName where show = genericShow

newtype Ident = Ident String
derive instance newtypeIdent :: Newtype Ident _
derive instance genericIdent :: Generic Ident _
derive instance eqIdent :: Eq Ident
derive instance ordIdent :: Ord Ident
instance showIdent :: Show Ident where show = genericShow

data DeclDataType = DeclDataTypeData | DeclDataTypeNewtype

newtype ImportDecl = ImportDecl
  { moduleName :: ModuleName
  , names :: Array Import
  , qualification :: Maybe ModuleName
  }
derive instance newtypeImportDecl :: Newtype ImportDecl _
derive instance genericImportDecl :: Generic ImportDecl _
derive instance eqImportDecl :: Eq ImportDecl
derive instance ordImportDecl :: Ord ImportDecl
instance showImportDecl :: Show ImportDecl where show = genericShow

foreign import kind OpNameType
foreign import data OpNameType_ValueOpName :: OpNameType
foreign import data OpNameType_TypeOpName :: OpNameType

-- Operator alias names
newtype OpName (proxy :: OpNameType) = OpName String
derive instance newtypeOpName :: Newtype (OpName proxy) _
derive instance genericOpName :: Generic (OpName proxy) _
derive instance eqOpName :: Eq (OpName proxy)
derive instance ordOpName :: Ord (OpName proxy)
instance showOpName :: Show (OpName proxy) where show = genericShow

foreign import kind ProperNameType
foreign import data ProperNameType_TypeName :: ProperNameType
foreign import data ProperNameType_ConstructorName :: ProperNameType
foreign import data ProperNameType_ClassName :: ProperNameType
foreign import data ProperNameType_KindName :: ProperNameType
foreign import data ProperNameType_Namespace :: ProperNameType

-- Proper names, i.e. capitalized names for e.g. module names, type/data constructors
newtype ProperName (proxy :: ProperNameType) = ProperName String
derive instance newtypeProperName :: Newtype (ProperName proxy) _
derive instance genericProperName :: Generic (ProperName proxy) _
derive instance eqProperName :: Eq (ProperName proxy)
derive instance ordProperName :: Ord (ProperName proxy)
instance showProperName :: Show (ProperName proxy) where show = genericShow

data DataMembers
  = DataAll
  | DataEnumerated (Array (ProperName ProperNameType_ConstructorName))
derive instance genericDataMembers :: Generic DataMembers _
derive instance eqDataMembers :: Eq DataMembers
derive instance ordDataMembers :: Ord DataMembers
instance showDataMembers :: Show DataMembers where show = genericShow

data Import
  = ImportValue Ident
  | ImportOp (OpName OpNameType_ValueOpName) -- e.g. "&&" function/value, rendered in parentheses
  | ImportType (ProperName ProperNameType_TypeName) (Maybe DataMembers) -- e.g. "CONSOLE", "Maybe"
  | ImportTypeOp (OpName OpNameType_TypeOpName) -- e.g. "<<<" type alias, rendered in parentheses as `type (<<<)`
  | ImportClass (ProperName ProperNameType_ClassName)
  | ImportKind (ProperName ProperNameType_KindName)
derive instance genericImport :: Generic Import _
derive instance eqImport :: Eq Import
derive instance ordImport :: Ord Import
instance showImport :: Show Import where show = genericShow

data Export
  = ExportValue Ident
  | ExportOp (OpName OpNameType_ValueOpName) -- e.g. "&&" function/value, rendered in parentheses
  | ExportType (ProperName ProperNameType_TypeName) (Maybe DataMembers) -- e.g. "CONSOLE", "Maybe"
  | ExportTypeOp (OpName OpNameType_TypeOpName) -- e.g. "<<<" type alias, rendered in parentheses as `type (<<<)`
  | ExportClass (ProperName ProperNameType_ClassName)
  | ExportKind (ProperName ProperNameType_KindName)
  | ExportModule ModuleName
derive instance genericExport :: Generic Export _
derive instance eqExport :: Eq Export
derive instance ordExport :: Ord Export
instance showExport :: Show Export where show = genericShow

data Declaration
  = DeclData DataHead (Array DataCtor)
  | DeclType DataHead Type
  -- | DeclNewtype DataHead (ProperName ProperNameType_ConstructorName) Type
  -- | DeclClass (ClassHead a) Array (Labeled Ident Type)
  -- | DeclInstanceChain (NonEmpty Array (Instance a))
  -- | DeclDerive {- (Maybe SourceToken) -} (InstanceHead a)
  -- | DeclSignature (Labeled Ident Type)
  -- | DeclValue (ValueBindingFields a)
  -- | DeclFixity FixityFields
  -- | DeclForeign (Foreign a)

data Type
  = TypeVar Ident
  | TypeConstructor (QualifiedName (ProperName ProperNameType_TypeName))
  | TypeWildcard
  | TypeHole Ident
  | TypeString String
  | TypeRow Row
  | TypeRecord Row
  | TypeApp Type Type
  | TypeForall (NonEmpty Array TypeVarBinding) Type
  | TypeArr Type Type
  | TypeKinded Type Kind
  | TypeOp Type (QualifiedName (OpName OpNameType_TypeOpName)) Type -- like TypeArr, but with custom type alias
  | TypeConstrained Constraint Type
  --
  -- no need to implement
  --
  -- | TypeOpName (QualifiedName (OpName OpNameType_TypeOpName))
  -- | TypeArrName
  -- | TypeParens Type

newtype QualifiedName a = QualifiedName
  { qualModule :: Maybe ModuleName
  , qualName :: a
  }
derive instance newtypeQualifiedName :: Newtype (QualifiedName a) _

-- Mu
data Kind
  = KindName (QualifiedName (ProperName ProperNameType_KindName))
  | KindArr Kind Kind
  | KindRow Kind
  | KindParens Kind

-- e.g. main , forAll
-- data Labeled a b = Labeled
--   { lblLabel :: a
--   , lblValue  :: b
--   }

data TypeVarBinding
  = TypeVarKinded Ident Kind
  | TypeVarName Ident

newtype DataHead = DataHead
  { dataHdName :: ProperName ProperNameType_TypeName
  , dataHdVars :: Array TypeVarBinding
  }
derive instance newtypeDataHead :: Newtype DataHead _

newtype DataCtor = DataCtor
  { dataCtorName :: ProperName ProperNameType_ConstructorName
  , dataCtorFields :: Array Type
  }
derive instance newtypeDataCtor :: Newtype DataCtor _

newtype Label = Label String

derive instance newtypeLabel :: Newtype Label _

newtype Row = Row
  { rowLabels :: Array { label :: Label, type_ :: Type }
  , rowTail :: Maybe Type
  }

derive instance newtypeRow :: Newtype Row _

data Constraint
  = Constraint
    { className :: QualifiedName (ProperName ProperNameType_ClassName)
    , args :: Array Type
    }
  | ConstraintParens Constraint

data OneOrDelimited a
  = OneOrDelimited_One a
  | OneOrDelimited_Many { first :: a, second :: a, tail :: Array a }

data ClassFundep
  = FundepDetermined (NonEmpty Array Ident)
  | FundepDetermines (NonEmpty Array Ident) (NonEmpty Array Ident)

-- Delimeted or separated
newtype ClassHead a = ClassHead
  { clsSuper :: Maybe (OneOrDelimited Constraint)
  , clsName :: ProperName ProperNameType_ClassName
  , clsVars :: Array TypeVarBinding
  , clsFundeps :: Array ClassFundep
  }

newtype ValueBindingFields = ValueBindingFields
  { valName :: Ident
  , valBinders :: Array Binder
  -- , valGuarded :: Guarded
  }

data RecordLabeled a
  = RecordPun Ident
  | RecordField Label a

data Binder
  = BinderWildcard
  | BinderVar Ident
  | BinderNamed Ident Binder
  | BinderConstructor (QualifiedName (ProperName ProperNameType_ConstructorName)) (Array Binder)
  | BinderBoolean Boolean
  | BinderChar Char
  | BinderString String
  | BinderNumber (Either Int Number)
  | BinderArray (Array Binder)
  | BinderRecord (Array (RecordLabeled Binder))
  | BinderParens Binder
  | BinderTyped Binder Type
  | BinderOp Binder (QualifiedName (OpName OpNameType_ValueOpName)) Binder

-- newtype Where = Where
--   { whereExpr :: Expr
--   , whereBindings :: Array LetBinding
--   }

-- data Guarded
--   = Unconditional Where
--   | Guarded (NonEmpty Array GuardedExpr)

-- newtype GuardedExpr = GuardedExpr
--   { grdBar :: SourceToken
--   , grdPatterns :: Separated PatternGuard
--   , grdSep :: SourceToken
--   , grdWhere :: Where
--   }

-- newtype PatternGuard = PatternGuard
--   { patBinder :: Maybe Binder
--   , patExpr :: Expr
--   }

-- data InstanceBinding
--   = InstanceBindingSignature (Labeled (Name Ident) Type)
--   | InstanceBindingName ValueBindingFields

-- newtype Instance = Instance
--   { instHead :: InstanceHead
--   , instBody :: Array InstanceBinding
--   }

newtype InstanceHead a = InstanceHead
  { instName :: Ident
  , instConstraints :: Maybe (OneOrDelimited Constraint)
  , instClass :: QualifiedName (ProperName ProperNameType_ClassName)
  , instTypes :: Array Type
  }

-- reservedNames :: Set String
-- reservedNames = Set.fromFoldable
--   [ "ado" , "case" , "class" , "data"
--   , "derive" , "do" , "else" , "false"
--   , "forall" , "foreign" , "import" , "if"
--   , "in" , "infix" , "infixl" , "infixr"
--   , "instance" , "let" , "module" , "newtype"
--   , "of" , "true" , "type" , "where"
--   ]
