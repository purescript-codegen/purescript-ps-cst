module Language.PS.AST.Types where

-- | This module is somewhat inspired by `purescript-cst` types.
-- | I've tried to preserve constructor names to simplify
-- | further "copy and paste based development".

import Prelude

import Data.Either (Either)
import Data.Either.Nested (type (\/))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.NonEmpty (NonEmpty)

-- | No need for imports list as they are collected from declarations
-- | during final codegen.
newtype Module = Module
  { moduleName :: ModuleName
  , imports :: Array ImportDecl
  , exports :: Array Export
  , declarations :: Array Declaration
  }

data Comments
  -- | Rendered as
  -- | ```purs
  -- | -- | line1
  -- | -- | line2
  -- | ```
  = OneLineComments (Array String)
  -- | Rendered as
  -- | ```purs
  -- | {-
  -- |   line1
  -- |   line2
  -- | -}
  -- | ```
  | BlockComments (Array String)

derive instance genericComments :: Generic Comments _
derive instance eqComments :: Eq Comments
derive instance ordComments :: Ord Comments

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
instance showOpName :: Show (OpName proxy) where
  show (OpName string) = "(OpName" <> show string <> ")"

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
  show (ProperName string) = "(ProperName" <> show string <> ")"

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
  = DeclData          { comments :: Maybe Comments, head :: DataHead, constructors :: Array DataCtor }
  | DeclType          { comments :: Maybe Comments, head :: DataHead, type_ :: Type }
  | DeclNewtype       { comments :: Maybe Comments, head :: DataHead, name :: ProperName ProperNameType_ConstructorName, type_ :: Type }
  | DeclClass         { comments :: Maybe Comments, head :: ClassHead, methods :: Array { ident :: Ident, type_ :: Type } }
  | DeclInstanceChain { comments :: Maybe Comments, instances :: NonEmpty Array Instance }
  | DeclDerive        { comments :: Maybe Comments, deriveType :: DeclDeriveType, head :: InstanceHead }
  | DeclSignature     { comments :: Maybe Comments, ident :: Ident, type_ :: Type }
  | DeclValue         { comments :: Maybe Comments, valueBindingFields :: ValueBindingFields }
  | DeclFixity        { comments :: Maybe Comments, fixityFields :: FixityFields }
  | DeclForeign       { comments :: Maybe Comments, foreign_ :: Foreign }
derive instance genericDeclaration :: Generic Declaration _
derive instance eqDeclaration :: Eq Declaration
derive instance ordDeclaration :: Ord Declaration
-- instance showDeclaration :: Show Declaration where show = genericShow

data DeclDeriveType
  = DeclDeriveType_Newtype
  | DeclDeriveType_Odrinary
derive instance genericDeclDeriveType :: Generic DeclDeriveType _
derive instance eqDeclDeriveType :: Eq DeclDeriveType
derive instance ordDeclDeriveType :: Ord DeclDeriveType

type InstanceHead =
  { instName :: Ident
  , instConstraints :: Array Constraint
  , instClass :: QualifiedName (ProperName ProperNameType_ClassName)
  , instTypes :: NonEmpty Array Type
  }

data Foreign
  = ForeignValue { ident :: Ident, type_ :: Type }
  | ForeignData { name :: ProperName ProperNameType_TypeName, kind_ :: Kind }
  | ForeignKind { name :: ProperName ProperNameType_KindName }
derive instance genericForeign :: Generic Foreign _
derive instance eqForeign :: Eq Foreign
derive instance ordForeign :: Ord Foreign

type FixityFieldsRow =
  ( keyword :: Fixity
  , precedence :: Int
  , operator :: FixityOp
  )

type FixityFields = Record FixityFieldsRow

data Fixity
  = Infix
  | Infixl
  | Infixr
derive instance genericFixity :: Generic Fixity _
derive instance eqFixity :: Eq Fixity
derive instance ordFixity :: Ord Fixity

data FixityOp
  = FixityValue (QualifiedName Ident \/ QualifiedName (ProperName ProperNameType_ConstructorName)) (OpName OpNameType_ValueOpName)
  | FixityType (QualifiedName (ProperName ProperNameType_TypeName)) (OpName OpNameType_TypeOpName)
derive instance genericFixityOp :: Generic FixityOp _
derive instance eqFixityOp :: Eq FixityOp
derive instance ordFixityOp :: Ord FixityOp

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
derive instance genericType :: Generic Type _
derive instance eqType :: Eq Type
derive instance ordType :: Ord Type
-- instance showType :: Show Type where show = genericShow

newtype QualifiedName a = QualifiedName
  { qualModule :: Maybe ModuleName
  , qualName :: a
  }
derive instance newtypeQualifiedName :: Newtype (QualifiedName a) _
derive instance eqQualifiedName :: Eq a => Eq (QualifiedName a)
derive instance ordQualifiedName :: Ord a => Ord (QualifiedName a)
instance showQualifiedName :: Show a => Show (QualifiedName a) where
  show (QualifiedName { qualModule, qualName }) = "(QualifiedName { qualModule = " <> show qualModule <> ", qualName = " <> show qualName <> " })"

data Kind
  = KindName (QualifiedName (ProperName ProperNameType_KindName))
  | KindArr Kind Kind
  | KindRow Kind
  -- | KindParens Kind -- no need
derive instance genericKind :: Generic Kind _
derive instance eqKind :: Eq Kind
derive instance ordKind :: Ord Kind
-- instance showKind :: Show Kind where
--   show _ = ""

data TypeVarBinding
  = TypeVarKinded Ident Kind
  | TypeVarName Ident
derive instance genericTypeVarBinding :: Generic TypeVarBinding _
derive instance eqTypeVarBinding :: Eq TypeVarBinding
derive instance ordTypeVarBinding :: Ord TypeVarBinding
-- instance showTypeVarBinding :: Show TypeVarBinding where show = genericShow

newtype DataHead = DataHead
  { dataHdName :: ProperName ProperNameType_TypeName
  , dataHdVars :: Array TypeVarBinding
  }
derive instance newtypeDataHead :: Newtype DataHead _
derive instance genericDataHead :: Generic DataHead _
derive instance eqDataHead :: Eq DataHead
derive instance ordDataHead :: Ord DataHead
-- instance showDataHead :: Show DataHead where show = genericShow

newtype DataCtor = DataCtor
  { dataCtorName :: ProperName ProperNameType_ConstructorName
  , dataCtorFields :: Array Type
  }
derive instance newtypeDataCtor :: Newtype DataCtor _
derive instance genericDataCtor :: Generic DataCtor _
derive instance eqDataCtor :: Eq DataCtor
derive instance ordDataCtor :: Ord DataCtor
-- instance showDataCtor :: Show DataCtor where show = genericShow

newtype Label = Label String
derive instance newtypeLabel :: Newtype Label _
derive instance genericLabel :: Generic Label _
derive instance eqLabel :: Eq Label
derive instance ordLabel :: Ord Label
instance showLabel :: Show Label where show = genericShow

newtype Row = Row
  { rowLabels :: Array { label :: Label, type_ :: Type }
  , rowTail :: Maybe Type
  }
derive instance newtypeRow :: Newtype Row _
derive instance genericRow :: Generic Row _
derive instance eqRow :: Eq Row
derive instance ordRow :: Ord Row
-- instance showRow :: Show Row where show = genericShow

newtype Constraint
  = Constraint
    { className :: QualifiedName (ProperName ProperNameType_ClassName)
    , args :: Array Type
    }
  -- | ConstraintParens Constraint
derive instance genericConstraint :: Generic Constraint _
derive instance eqConstraint :: Eq Constraint
derive instance ordConstraint :: Ord Constraint
-- instance showConstraint :: Show Constraint where show = genericShow

data ClassFundep
  = FundepDetermines (NonEmpty Array Ident) (NonEmpty Array Ident)
  -- | FundepDetermined (NonEmpty Array Ident) -- parser is not allowing it (i.e. `class Foo a | a`)?
derive instance genericClassFundep :: Generic ClassFundep _
derive instance eqClassFundep :: Eq ClassFundep
derive instance ordClassFundep :: Ord ClassFundep
instance showClassFundep :: Show ClassFundep where show = genericShow

-- Delimeted or separated
type ClassHead =
  { name :: ProperName ProperNameType_ClassName
  , vars :: Array TypeVarBinding
  , super :: Array Constraint
  , fundeps :: Array ClassFundep
  }

type ValueBindingFieldsRow =
  ( name :: Ident
  , binders :: Array Binder
  , guarded :: Guarded
  )

type ValueBindingFields = Record ValueBindingFieldsRow

data RecordLabeled a
  = RecordPun Ident
  | RecordField Label a
derive instance genericRecordLabeled :: Generic (RecordLabeled a) _
derive instance eqRecordLabeled :: Eq a => Eq (RecordLabeled a)
derive instance ordRecordLabeled :: Ord a => Ord (RecordLabeled a)
instance showRecordLabeled :: Show a => Show (RecordLabeled a) where
  show (RecordPun ident) = "(RecordPun " <> show ident <> ")"
  show (RecordField label a) = "(RecordField { label = " <> show label <> ", a = " <> show a <> " })"

data Binder
  = BinderWildcard
  | BinderVar Ident
  | BinderNamed { ident :: Ident, binder :: Binder }
  | BinderConstructor { name :: QualifiedName (ProperName ProperNameType_ConstructorName), args :: Array Binder }
  | BinderBoolean Boolean
  | BinderChar Char
  | BinderString String
  | BinderNumber (Either Int Number)
  | BinderArray (Array Binder)
  | BinderRecord (Array (RecordLabeled Binder))
  -- | BinderParens Binder -- no need
  | BinderTyped Binder Type
  | BinderOp Binder (QualifiedName (OpName OpNameType_ValueOpName)) Binder
derive instance genericBinder :: Generic Binder _
derive instance eqBinder :: Eq Binder
derive instance ordBinder :: Ord Binder
-- instance showBinder :: Show Binder where show = genericShow

data Guarded
  = Unconditional Where
  | Guarded (NonEmpty Array GuardedExpr)
derive instance genericGuarded :: Generic Guarded _
derive instance eqGuarded :: Eq Guarded
derive instance ordGuarded :: Ord Guarded

type Where =
  { expr :: Expr
  , whereBindings :: Array LetBinding
  }

data LetBinding
  = LetBindingSignature { ident :: Ident, type_ :: Type }
  | LetBindingName ValueBindingFields
  | LetBindingPattern { binder :: Binder, where_ :: Where }
derive instance genericLetBinding :: Generic LetBinding _
derive instance eqLetBinding :: Eq LetBinding
derive instance ordLetBinding :: Ord LetBinding

type GuardedExpr =
  { patterns :: NonEmpty Array PatternGuard
  , where_ :: Where
  }

type PatternGuard =
  { binder :: Maybe Binder
  , expr :: Expr
  }

data Expr
  = ExprHole Ident
  | ExprSection
  | ExprIdent (QualifiedName Ident)
  | ExprConstructor (QualifiedName (ProperName ProperNameType_ConstructorName))
  | ExprBoolean Boolean
  | ExprChar Char
  | ExprString String
  | ExprNumber (Either Int Number)
  | ExprArray (Array Expr)
  | ExprRecord (Array (RecordLabeled Expr))
  -- | ExprParens Expr -- no need
  | ExprTyped Expr Type
  | ExprInfix Expr Expr Expr -- e.g. `1 : 2 : Nil`
  | ExprOp Expr (QualifiedName (OpName OpNameType_ValueOpName)) Expr
  | ExprOpName (QualifiedName (OpName OpNameType_ValueOpName))
  | ExprNegate Expr -- ????
  | ExprRecordAccessor RecordAccessor
  | ExprRecordUpdate Expr (NonEmpty Array RecordUpdate)
  | ExprApp Expr Expr
  | ExprLambda Lambda
  | ExprIf IfThenElse
  | ExprCase CaseOf
  | ExprLet LetIn
  | ExprDo DoBlock
  | ExprAdo AdoBlock
derive instance genericExpr :: Generic Expr _
derive instance eqExpr :: Eq Expr
derive instance ordExpr :: Ord Expr

type RecordAccessor =
  { recExpr :: Expr
  , recPath :: NonEmpty Array Label
  }

data RecordUpdate
  = RecordUpdateLeaf Label Expr
  | RecordUpdateBranch Label (NonEmpty Array RecordUpdate)
derive instance genericRecordUpdate :: Generic RecordUpdate _
derive instance eqRecordUpdate :: Eq RecordUpdate
derive instance ordRecordUpdate :: Ord RecordUpdate

type Lambda =
  { binders :: NonEmpty Array Binder
  , body :: Expr
  }

type IfThenElse =
  { cond :: Expr
  , true_ :: Expr
  , false_ :: Expr
  }

type CaseOf =
  { head :: NonEmpty Array Expr
  , branches :: NonEmpty Array { binders :: NonEmpty Array Binder, body :: Guarded }
  }

type LetIn =
  { body :: Expr
  , bindings :: NonEmpty Array LetBinding
  }

type DoBlock = NonEmpty Array DoStatement

data DoStatement
  = DoLet (NonEmpty Array LetBinding)
  | DoDiscard Expr
  | DoBind { binder :: Binder, expr :: Expr }
derive instance genericDoStatement :: Generic DoStatement _
derive instance eqDoStatement :: Eq DoStatement
derive instance ordDoStatement :: Ord DoStatement

type AdoBlock =
  { statements :: Array DoStatement
  , result :: Expr
  }

data InstanceBinding
  = InstanceBindingSignature { ident :: Ident, type_ :: Type }
  | InstanceBindingName ValueBindingFields
derive instance genericInstanceBinding :: Generic InstanceBinding _
derive instance eqInstanceBinding :: Eq InstanceBinding
derive instance ordInstanceBinding :: Ord InstanceBinding

type Instance =
  { head :: InstanceHead
  , body :: Array InstanceBinding
  }

-- TODO: add `_` postfix during printing for reservedNames
-- reservedNames :: Set String
-- reservedNames = Set.fromFoldable
--   [ "ado" , "case" , "class" , "data"
--   , "derive" , "do" , "else" , "false"
--   , "forall" , "foreign" , "import" , "if"
--   , "in" , "infix" , "infixl" , "infixr"
--   , "instance" , "let" , "module" , "newtype"
--   , "of" , "true" , "type" , "where"
--   ]

infixl 5 ExprLambda as ====>
infixr 5 TypeArr as ====>>
infixr 5 KindArr as ====>>>
