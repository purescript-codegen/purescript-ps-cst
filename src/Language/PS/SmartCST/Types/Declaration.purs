module Language.PS.SmartCST.Types.Declaration where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either)
import Data.Either.Nested (type (\/))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Language.PS.CST.Types.Leafs (ClassFundep, Comments, DeclDeriveType, Fixity, Ident, Label, OpName, OpNameType_TypeOpName, OpNameType_ValueOpName, ProperName, ProperNameType_ClassName, ProperNameType_ConstructorName, ProperNameType_KindName, ProperNameType_TypeName, RecordLabeled)
import Language.PS.SmartCST.Types.SmartQualifiedName (SmartQualifiedName)
import Language.PS.SmartCST.Types.SmartQualifiedNameConstructor (SmartQualifiedNameConstructor)

data Declaration
  = DeclData
    { comments :: Maybe Comments
    , head :: DataHead
    , constructors :: Array DataCtor
    }
  | DeclType
    { comments :: Maybe Comments
    , head :: DataHead
    , type_ :: Type
    }
  | DeclNewtype
    { comments :: Maybe Comments
    , head :: DataHead
    , name :: ProperName ProperNameType_ConstructorName
    , type_ :: Type
    }
  | DeclClass
    { comments :: Maybe Comments
    , head :: ClassHead
    , methods :: Array { ident :: Ident, type_ :: Type }
    }
  | DeclInstanceChain
    { comments :: Maybe Comments
    , instances :: NonEmptyArray (Instance)
    }
  | DeclDerive
    { comments :: Maybe Comments
    , deriveType :: DeclDeriveType
    , head :: InstanceHead
    }
  | DeclSignature
    { comments :: Maybe Comments
    , ident :: Ident
    , type_ :: Type
    }
  | DeclValue
    { comments :: Maybe Comments
    , valueBindingFields :: ValueBindingFields
    }
  | DeclFixity
    { comments :: Maybe Comments
    , fixityFields :: FixityFields
    }
  | DeclForeign
    { comments :: Maybe Comments
    , foreign_ :: Foreign
    }

derive instance genericDeclaration :: Generic Declaration _
derive instance eqDeclaration :: Eq Declaration
derive instance ordDeclaration :: Ord Declaration
instance showDeclaration :: Show Declaration where show = genericShow

type InstanceHead =
  { instName :: Ident
  , instConstraints :: Array Constraint
  , instClass :: SmartQualifiedName (ProperName ProperNameType_ClassName)
  , instTypes :: NonEmptyArray Type
  }

data Foreign
  = ForeignValue { ident :: Ident, type_ :: Type }
  | ForeignData { name :: ProperName ProperNameType_TypeName, kind_ :: Kind }
  | ForeignKind { name :: ProperName ProperNameType_KindName }

derive instance genericForeign :: Generic Foreign _
derive instance eqForeign :: Eq Foreign
derive instance ordForeign :: Ord Foreign
instance showForeign :: Show Foreign where show = genericShow

type FixityFields =
  { keyword :: Fixity
  , precedence :: Int
  , operator :: FixityOp
  }

data FixityOp
  = FixityValue (SmartQualifiedName Ident \/ SmartQualifiedNameConstructor) (OpName OpNameType_ValueOpName)
  | FixityType (SmartQualifiedName (ProperName ProperNameType_TypeName)) (OpName OpNameType_TypeOpName)

derive instance genericFixityOp :: Generic FixityOp _
derive instance eqFixityOp :: Eq FixityOp
derive instance ordFixityOp :: Ord FixityOp
instance showFixityOp :: Show FixityOp where show = genericShow

data Type
  = TypeVar Ident
  | TypeConstructor (SmartQualifiedName (ProperName ProperNameType_TypeName))
  | TypeWildcard
  | TypeHole Ident
  | TypeString String
  | TypeRow Row
  | TypeRecord Row
  | TypeApp Type Type
  | TypeForall (NonEmptyArray TypeVarBinding) Type
  | TypeArr Type Type
  | TypeKinded Type Kind
  | TypeOp Type (SmartQualifiedName (OpName OpNameType_TypeOpName)) Type -- like TypeArr, but with custom type alias
  | TypeConstrained Constraint Type
  --
  -- no need to implement
  --
  -- | TypeOpName (SmartQualifiedName (OpName OpNameType_TypeOpName))
  -- | TypeArrName
  -- | TypeParens Type

derive instance genericType :: Generic Type _
derive instance eqType :: Eq Type
derive instance ordType :: Ord Type
instance showType :: Show Type where show x = genericShow x

data Kind
  = KindName (SmartQualifiedName (ProperName ProperNameType_KindName))
  | KindArr Kind Kind
  | KindRow Kind
  -- | KindParens Kind -- no need

derive instance genericKind :: Generic Kind _
derive instance eqKind :: Eq Kind
derive instance ordKind :: Ord Kind
instance showKind :: Show Kind where show x = genericShow x

data TypeVarBinding
  = TypeVarKinded Ident Kind
  | TypeVarName Ident

derive instance genericTypeVarBinding :: Generic TypeVarBinding _
derive instance eqTypeVarBinding :: Eq TypeVarBinding
derive instance ordTypeVarBinding :: Ord TypeVarBinding
instance showTypeVarBinding :: Show TypeVarBinding where show = genericShow

newtype DataHead = DataHead
  { dataHdName :: ProperName ProperNameType_TypeName
  , dataHdVars :: Array TypeVarBinding
  }

derive instance newtypeDataHead :: Newtype DataHead _
derive instance genericDataHead :: Generic DataHead _
derive instance eqDataHead :: Eq DataHead
derive instance ordDataHead :: Ord DataHead
instance showDataHead :: Show DataHead where show = genericShow

newtype DataCtor = DataCtor
  { dataCtorName :: ProperName ProperNameType_ConstructorName
  , dataCtorFields :: Array Type
  }

derive instance newtypeDataCtor :: Newtype DataCtor _
derive instance genericDataCtor :: Generic DataCtor _
derive instance eqDataCtor :: Eq DataCtor
derive instance ordDataCtor :: Ord DataCtor
instance showDataCtor :: Show DataCtor where show = genericShow

type Row =
  { rowLabels :: Array { label :: Label, type_ :: Type }
  , rowTail :: Maybe Type
  }

newtype Constraint
  = Constraint
    { className :: SmartQualifiedName (ProperName ProperNameType_ClassName)
    , args :: Array Type
    }
  -- | ConstraintParens Constraint

derive instance genericConstraint :: Generic Constraint _
derive instance eqConstraint :: Eq Constraint
derive instance ordConstraint :: Ord Constraint
instance showConstraint :: Show Constraint where show = genericShow

-- Delimeted or separated
type ClassHead =
  { name :: ProperName ProperNameType_ClassName
  , vars :: Array TypeVarBinding
  , super :: Array Constraint
  , fundeps :: Array ClassFundep
  }

type ValueBindingFields =
  { name :: Ident
  , binders :: Array Binder
  , guarded :: Guarded
  }

data Binder
  = BinderWildcard
  | BinderVar Ident
  | BinderNamed { ident :: Ident, binder :: Binder }
  | BinderConstructor { name :: SmartQualifiedNameConstructor, args :: Array Binder }
  | BinderBoolean Boolean
  | BinderChar Char
  | BinderString String
  | BinderNumber (Either Int Number)
  | BinderArray (Array Binder)
  | BinderRecord (Array (RecordLabeled Binder))
  | BinderTyped Binder Type
  | BinderOp Binder (SmartQualifiedName (OpName OpNameType_ValueOpName)) Binder
  -- | BinderParens Binder -- no need

derive instance genericBinder :: Generic Binder _
derive instance eqBinder :: Eq Binder
derive instance ordBinder :: Ord Binder
instance showBinder :: Show Binder where show x = genericShow x

data Guarded
  = Unconditional (Where)
  | Guarded (NonEmptyArray GuardedExpr)

derive instance genericGuarded :: Generic Guarded _
derive instance eqGuarded :: Eq Guarded
derive instance ordGuarded :: Ord Guarded
instance showGuarded :: Show Guarded where show x = genericShow x

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
instance showLetBinding :: Show LetBinding where show x = genericShow x

type GuardedExpr =
  { patterns :: NonEmptyArray PatternGuard
  , where_ :: Where
  }

type PatternGuard =
  { binder :: Maybe Binder
  , expr :: Expr
  }

data Expr
  = ExprHole Ident
  | ExprSection
  | ExprIdent (SmartQualifiedName Ident)
  | ExprVar Ident -- like ExprIdent, but without import
  | ExprConstructor (SmartQualifiedNameConstructor)
  | ExprBoolean Boolean
  | ExprChar Char
  | ExprString String
  | ExprNumber (Either Int Number)
  | ExprArray (Array Expr)
  | ExprRecord (Array (RecordLabeled Expr))
  | ExprTyped Expr Type
  | ExprInfix Expr Expr Expr -- e.g. `1 : 2 : Nil`
  | ExprOp Expr (SmartQualifiedName (OpName OpNameType_ValueOpName)) Expr
  | ExprOpName (SmartQualifiedName (OpName OpNameType_ValueOpName))
  | ExprNegate Expr -- ????
  | ExprRecordAccessor RecordAccessor
  | ExprRecordUpdate Expr (NonEmptyArray RecordUpdate)
  | ExprApp Expr Expr
  | ExprLambda Lambda
  | ExprIf IfThenElse
  | ExprCase CaseOf
  | ExprLet LetIn
  | ExprDo (NonEmptyArray DoStatement)
  | ExprAdo AdoBlock
  -- | ExprParens Expr -- no need

derive instance genericExpr :: Generic Expr _
derive instance eqExpr :: Eq Expr
derive instance ordExpr :: Ord Expr
instance showExpr :: Show Expr where show x = genericShow x

type RecordAccessor =
  { recExpr :: Expr
  , recPath :: NonEmptyArray Label
  }

data RecordUpdate
  = RecordUpdateLeaf Label Expr
  | RecordUpdateBranch Label (NonEmptyArray RecordUpdate)

derive instance genericRecordUpdate :: Generic RecordUpdate _
derive instance eqRecordUpdate :: Eq RecordUpdate
derive instance ordRecordUpdate :: Ord RecordUpdate
instance showRecordUpdate :: Show RecordUpdate where show x = genericShow x

type Lambda =
  { binders :: NonEmptyArray Binder
  , body :: Expr
  }

type IfThenElse =
  { cond :: Expr
  , true_ :: Expr
  , false_ :: Expr
  }

type CaseOf =
  { head :: NonEmptyArray Expr
  , branches :: NonEmptyArray { binders :: NonEmptyArray Binder, body :: Guarded }
  }

type LetIn =
  { body :: Expr
  , bindings :: NonEmptyArray LetBinding
  }

data DoStatement
  = DoLet (NonEmptyArray LetBinding)
  | DoDiscard Expr
  | DoBind { binder :: Binder, expr :: Expr }

derive instance genericDoStatement :: Generic DoStatement _
derive instance eqDoStatement :: Eq DoStatement
derive instance ordDoStatement :: Ord DoStatement
instance showDoStatement :: Show DoStatement where show = genericShow

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
instance showInstanceBinding :: Show InstanceBinding where show = genericShow

type Instance =
  { head :: InstanceHead
  , body :: Array InstanceBinding
  }

infixl 5 ExprLambda as ====>
infixr 5 TypeArr as ====>>
infixr 5 KindArr as ====>>>
