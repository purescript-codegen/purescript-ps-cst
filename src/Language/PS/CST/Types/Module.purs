module Language.PS.CST.Types.Module where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

import Language.PS.CST.Types.Leafs (Ident, ModuleName, OpName, OpNameType_TypeOpName, OpNameType_ValueOpName, ProperName, ProperNameType_ClassName, ProperNameType_ConstructorName, ProperNameType_KindName, ProperNameType_TypeName)
import Language.PS.CST.Types.Declaration (Declaration)

newtype Module = Module
  { moduleName :: ModuleName
  , imports :: Array ImportDecl
  , exports :: Array Export
  , declarations :: Array Declaration
  }
derive instance newtypeModule :: Newtype Module _
derive instance genericModule :: Generic Module _
derive instance eqModule :: Eq Module
derive instance ordModule :: Ord Module
instance showModule :: Show Module where show = genericShow

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
