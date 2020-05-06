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
  -- , exports :: Array ExportDecl
  -- , declarations :: Array Declaration
  }

newtype ModuleName = ModuleName (NonEmpty Array (ProperName ProperNameType_Namespace))
derive instance newtypeModuleName :: Newtype ModuleName _
derive instance genericModuleName :: Generic ModuleName _
derive instance eqModuleName :: Eq ModuleName
derive instance ordModuleName :: Ord ModuleName
instance showModuleName :: Show ModuleName where
  show = genericShow

newtype Ident = Ident String
derive instance genericIdent :: Generic Ident _
derive instance eqIdent :: Eq Ident
derive instance ordIdent :: Ord Ident
instance showIdent :: Show Ident where
  show = genericShow

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
instance showImportDecl :: Show ImportDecl where
  show = genericShow

foreign import kind OpNameType
foreign import data OpNameType_ValueOpName :: OpNameType
foreign import data OpNameType_TypeOpName :: OpNameType

-- Operator alias names
newtype OpName (proxy :: OpNameType) = OpName String
derive instance newtypeOpName :: Newtype (OpName proxy) _
derive instance genericOpName :: Generic (OpName proxy) _
derive instance eqOpName :: Eq (OpName proxy)
derive instance ordOpName :: Ord (OpName proxy)
instance showOpName :: Show (OpName proxy) where
  show = genericShow

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
instance showProperName :: Show (ProperName proxy) where
  show = genericShow

data DataMembers
  = DataAll
  | DataEnumerated (Array (ProperName ProperNameType_ConstructorName))
derive instance genericDataMembers :: Generic DataMembers _
derive instance eqDataMembers :: Eq DataMembers
derive instance ordDataMembers :: Ord DataMembers
instance showDataMembers :: Show DataMembers where
  show = genericShow

data Import
  = ImportValue Ident
  | ImportOp (OpName OpNameType_ValueOpName) -- e.g. "&&" function/value, rendered in parentheses
  | ImportType (ProperName ProperNameType_TypeName) (Maybe DataMembers) -- e.g. "CONSOLE", "Maybe"
  | ImportTypeOp (OpName OpNameType_TypeOpName) -- e.g. "<<<" type alias, rendered in parentheses as `type (<<<)`
  | ImportClass (ProperName ProperNameType_ClassName)
  | ImportKind (ProperName ProperNameType_KindName)
derive instance newtypeIdent :: Newtype Ident _
derive instance genericImport :: Generic Import _
derive instance eqImport :: Eq Import
derive instance ordImport :: Ord Import
instance showImport :: Show Import where
  show = genericShow

reservedNames :: Set String
reservedNames = Set.fromFoldable
  [ "ado" , "case" , "class" , "data"
  , "derive" , "do" , "else" , "false"
  , "forall" , "foreign" , "import" , "if"
  , "in" , "infix" , "infixl" , "infixr"
  , "instance" , "let" , "module" , "newtype"
  , "of" , "true" , "type" , "where"
  ]
