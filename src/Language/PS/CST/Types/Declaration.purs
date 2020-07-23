module Language.PS.CST.Types.Declaration where

import Prelude

import Control.Monad.State (State, modify_)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Maybe (fromMaybe) as Maybe
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple.Nested
import Data.Either.Nested
import Language.PS.CST.Types.Leafs

data Declaration qualifiedName
  = DeclData
    { comments :: Maybe Comments
    , head :: DataHead qualifiedName
    , constructors :: Array (DataCtor qualifiedName)
    }
  | DeclType
    { comments :: Maybe Comments
    , head :: DataHead qualifiedName
    , type_ :: Type qualifiedName
    }
  | DeclNewtype
    { comments :: Maybe Comments
    , head :: DataHead qualifiedName
    , name :: ProperName ProperNameType_ConstructorName
    , type_ :: Type qualifiedName
    }
  | DeclClass
    { comments :: Maybe Comments
    , head :: ClassHead qualifiedName
    , methods :: Array { ident :: Ident, type_ :: Type qualifiedName }
    }
  | DeclInstanceChain
    { comments :: Maybe Comments
    , instances :: NonEmptyArray (Instance qualifiedName)
    }
  | DeclDerive
    { comments :: Maybe Comments
    , deriveType :: DeclDeriveType
    , head :: InstanceHead qualifiedName
    }
  | DeclSignature
    { comments :: Maybe Comments
    , ident :: Ident
    , type_ :: Type qualifiedName
    }
  | DeclValue
    { comments :: Maybe Comments
    , valueBindingFields :: ValueBindingFields qualifiedName
    }
  | DeclFixity
    { comments :: Maybe Comments
    , fixityFields :: FixityFields qualifiedName
    }
  | DeclForeign
    { comments :: Maybe Comments
    , foreign_ :: Foreign qualifiedName
    }
-- | derive instance genericDeclaration :: Generic Declaration _
-- | derive instance eqDeclaration :: Eq Declaration
-- | derive instance ordDeclaration :: Ord Declaration
-- instance showDeclaration :: Show Declaration where show = genericShow

type InstanceHead qualifiedName =
  { instName :: Ident
  , instConstraints :: Array (Constraint qualifiedName)
  , instClass :: qualifiedName (ProperName ProperNameType_ClassName)
  , instTypes :: NonEmptyArray (Type qualifiedName)
  }

data Foreign qualifiedName
  = ForeignValue { ident :: Ident, type_ :: Type qualifiedName }
  | ForeignData { name :: ProperName ProperNameType_TypeName, kind_ :: Kind qualifiedName }
  | ForeignKind { name :: ProperName ProperNameType_KindName }
-- | derive instance genericForeign :: Generic Foreign _
-- | derive instance eqForeign :: Eq Foreign
-- | derive instance ordForeign :: Ord Foreign

type FixityFields qualifiedName =
  { keyword :: Fixity
  , precedence :: Int
  , operator :: FixityOp qualifiedName
  }

data FixityOp qualifiedName
  = FixityValue (qualifiedName Ident \/ qualifiedName (ProperName ProperNameType_ConstructorName)) (OpName OpNameType_ValueOpName)
  | FixityType (qualifiedName (ProperName ProperNameType_TypeName)) (OpName OpNameType_TypeOpName)
-- | derive instance genericFixityOp :: Generic FixityOp _
-- | derive instance eqFixityOp :: Eq FixityOp
-- | derive instance ordFixityOp :: Ord FixityOp

data Type qualifiedName
  = TypeVar Ident
  | TypeConstructor (qualifiedName (ProperName ProperNameType_TypeName))
  | TypeWildcard
  | TypeHole Ident
  | TypeString String
  | TypeRow (Row qualifiedName)
  | TypeRecord (Row qualifiedName)
  | TypeApp (Type qualifiedName) (Type qualifiedName)
  | TypeForall (NonEmptyArray (TypeVarBinding qualifiedName)) (Type qualifiedName)
  | TypeArr (Type qualifiedName) (Type qualifiedName)
  | TypeKinded (Type qualifiedName) (Kind qualifiedName)
  | TypeOp (Type qualifiedName) (qualifiedName (OpName OpNameType_TypeOpName)) (Type qualifiedName) -- like TypeArr, but with custom type alias
  | TypeConstrained (Constraint qualifiedName) (Type qualifiedName)
  --
  -- no need to implement
  --
  -- | TypeOpName (qualifiedName (OpName OpNameType_TypeOpName))
  -- | TypeArrName
  -- | TypeParens Type
-- | derive instance genericType :: Generic Type _
-- | derive instance eqType :: Eq Type
-- | derive instance ordType :: Ord Type
-- instance showType :: Show Type where show = genericShow

data Kind qualifiedName
  = KindName (qualifiedName (ProperName ProperNameType_KindName))
  | KindArr (Kind qualifiedName) (Kind qualifiedName)
  | KindRow (Kind qualifiedName)
  -- | KindParens Kind -- no need
-- | derive instance genericKind :: Generic Kind _
-- | derive instance eqKind :: Eq Kind
-- | derive instance ordKind :: Ord Kind
-- instance showKind :: Show Kind where
--   show _ = ""

data TypeVarBinding qualifiedName
  = TypeVarKinded Ident (Kind qualifiedName)
  | TypeVarName Ident
-- | derive instance genericTypeVarBinding :: Generic TypeVarBinding _
-- | derive instance eqTypeVarBinding :: Eq TypeVarBinding
-- | derive instance ordTypeVarBinding :: Ord TypeVarBinding
-- instance showTypeVarBinding :: Show TypeVarBinding where show = genericShow

newtype DataHead qualifiedName = DataHead
  { dataHdName :: ProperName ProperNameType_TypeName
  , dataHdVars :: Array (TypeVarBinding qualifiedName)
  }
-- | derive instance newtypeDataHead :: Newtype DataHead _
-- | derive instance genericDataHead :: Generic DataHead _
-- | derive instance eqDataHead :: Eq DataHead
-- | derive instance ordDataHead :: Ord DataHead
-- instance showDataHead :: Show DataHead where show = genericShow

newtype DataCtor qualifiedName = DataCtor
  { dataCtorName :: ProperName ProperNameType_ConstructorName
  , dataCtorFields :: Array (Type qualifiedName)
  }
-- | derive instance newtypeDataCtor :: Newtype DataCtor _
-- | derive instance genericDataCtor :: Generic DataCtor _
-- | derive instance eqDataCtor :: Eq DataCtor
-- | derive instance ordDataCtor :: Ord DataCtor
-- instance showDataCtor :: Show DataCtor where show = genericShow

newtype Row qualifiedName = Row
  { rowLabels :: Array { label :: Label, type_ :: Type qualifiedName }
  , rowTail :: Maybe (Type qualifiedName)
  }
-- | derive instance newtypeRow :: Newtype Row _
-- | derive instance genericRow :: Generic Row _
-- | derive instance eqRow :: Eq Row
-- | derive instance ordRow :: Ord Row
-- instance showRow :: Show Row where show = genericShow

newtype Constraint qualifiedName
  = Constraint
    { className :: qualifiedName (ProperName ProperNameType_ClassName)
    , args :: Array (Type qualifiedName)
    }
  -- | ConstraintParens Constraint
-- | derive instance genericConstraint :: Generic Constraint _
-- | derive instance eqConstraint :: Eq Constraint
-- | derive instance ordConstraint :: Ord Constraint
-- instance showConstraint :: Show Constraint where show = genericShow

-- Delimeted or separated
type ClassHead qualifiedName =
  { name :: ProperName ProperNameType_ClassName
  , vars :: Array (TypeVarBinding qualifiedName)
  , super :: Array (Constraint qualifiedName)
  , fundeps :: Array ClassFundep
  }

type ValueBindingFields qualifiedName =
  { name :: Ident
  , binders :: Array (Binder qualifiedName)
  , guarded :: Guarded qualifiedName
  }

data Binder qualifiedName
  = BinderWildcard
  | BinderVar Ident
  | BinderNamed { ident :: Ident, binder :: Binder qualifiedName }
  | BinderConstructor { name :: qualifiedName (ProperName ProperNameType_ConstructorName), args :: Array (Binder qualifiedName) }
  | BinderBoolean Boolean
  | BinderChar Char
  | BinderString String
  | BinderNumber (Either Int Number)
  | BinderArray (Array (Binder qualifiedName))
  | BinderRecord (Array (RecordLabeled (Binder qualifiedName)))
  | BinderTyped (Binder qualifiedName) (Type qualifiedName)
  | BinderOp (Binder qualifiedName) (qualifiedName (OpName OpNameType_ValueOpName)) (Binder qualifiedName)
  -- | BinderParens Binder -- no need

-- | derive instance genericBinder :: Generic Binder _
-- | derive instance eqBinder :: Eq Binder
-- | derive instance ordBinder :: Ord Binder
-- instance showBinder :: Show Binder where show = genericShow

data Guarded qualifiedName
  = Unconditional (Where qualifiedName)
  | Guarded (NonEmptyArray (GuardedExpr qualifiedName))
-- | derive instance genericGuarded :: Generic Guarded _
-- | derive instance eqGuarded :: Eq Guarded
-- | derive instance ordGuarded :: Ord Guarded

type Where qualifiedName =
  { expr :: Expr qualifiedName
  , whereBindings :: Array (LetBinding qualifiedName)
  }

data LetBinding qualifiedName
  = LetBindingSignature { ident :: Ident, type_ :: Type qualifiedName }
  | LetBindingName (ValueBindingFields qualifiedName)
  | LetBindingPattern { binder :: Binder qualifiedName, where_ :: Where qualifiedName }
-- | derive instance genericLetBinding :: Generic (LetBinding qualifiedName) _
-- | derive instance eqLetBinding :: Eq (LetBinding qualifiedName)
-- | derive instance ordLetBinding :: Ord (LetBinding qualifiedName)

type GuardedExpr qualifiedName =
  { patterns :: NonEmptyArray (PatternGuard qualifiedName)
  , where_ :: Where qualifiedName
  }

type PatternGuard qualifiedName =
  { binder :: Maybe (Binder qualifiedName)
  , expr :: Expr qualifiedName
  }

data Expr qualifiedName
  = ExprHole Ident
  | ExprSection
  | ExprIdent (qualifiedName Ident)
  | ExprConstructor (qualifiedName (ProperName ProperNameType_ConstructorName))
  | ExprBoolean Boolean
  | ExprChar Char
  | ExprString String
  | ExprNumber (Either Int Number)
  | ExprArray (Array (Expr qualifiedName))
  | ExprRecord (Array (RecordLabeled (Expr qualifiedName)))
  | ExprTyped (Expr qualifiedName) (Type qualifiedName)
  | ExprInfix (Expr qualifiedName) (Expr qualifiedName) (Expr qualifiedName) -- e.g. `1 : 2 : Nil`
  | ExprOp (Expr qualifiedName) (qualifiedName (OpName OpNameType_ValueOpName)) (Expr qualifiedName)
  | ExprOpName (qualifiedName (OpName OpNameType_ValueOpName))
  | ExprNegate (Expr qualifiedName) -- ????
  | ExprRecordAccessor (RecordAccessor qualifiedName)
  | ExprRecordUpdate (Expr qualifiedName) (NonEmptyArray (RecordUpdate qualifiedName))
  | ExprApp (Expr qualifiedName) (Expr qualifiedName)
  | ExprLambda (Lambda qualifiedName)
  | ExprIf (IfThenElse qualifiedName)
  | ExprCase (CaseOf qualifiedName)
  | ExprLet (LetIn qualifiedName)
  | ExprDo (NonEmptyArray (DoStatement qualifiedName))
  | ExprAdo (AdoBlock qualifiedName)
  -- | ExprParens Expr -- no need
-- | derive instance genericExpr :: Generic Expr _
-- | derive instance eqExpr :: Eq Expr
-- | derive instance ordExpr :: Ord Expr

type RecordAccessor qualifiedName =
  { recExpr :: (Expr qualifiedName)
  , recPath :: NonEmptyArray Label
  }

data RecordUpdate qualifiedName
  = RecordUpdateLeaf Label (Expr qualifiedName)
  | RecordUpdateBranch Label (NonEmptyArray (RecordUpdate qualifiedName))
-- | derive instance genericRecordUpdate :: Generic RecordUpdate _
-- | derive instance eqRecordUpdate :: Eq RecordUpdate
-- | derive instance ordRecordUpdate :: Ord RecordUpdate

type Lambda qualifiedName =
  { binders :: NonEmptyArray (Binder qualifiedName)
  , body :: Expr qualifiedName
  }

type IfThenElse qualifiedName =
  { cond :: Expr qualifiedName
  , true_ :: Expr qualifiedName
  , false_ :: Expr qualifiedName
  }

type CaseOf qualifiedName =
  { head :: NonEmptyArray (Expr qualifiedName)
  , branches :: NonEmptyArray { binders :: NonEmptyArray (Binder qualifiedName), body :: Guarded qualifiedName }
  }

type LetIn qualifiedName =
  { body :: Expr qualifiedName
  , bindings :: NonEmptyArray (LetBinding qualifiedName)
  }

data DoStatement qualifiedName
  = DoLet (NonEmptyArray (LetBinding qualifiedName))
  | DoDiscard (Expr qualifiedName)
  | DoBind { binder :: Binder qualifiedName, expr :: Expr qualifiedName }
-- | derive instance genericDoStatement :: Generic DoStatement _
-- | derive instance eqDoStatement :: Eq DoStatement
-- | derive instance ordDoStatement :: Ord DoStatement

type AdoBlock qualifiedName =
  { statements :: Array (DoStatement qualifiedName)
  , result :: Expr qualifiedName
  }

data InstanceBinding qualifiedName
  = InstanceBindingSignature { ident :: Ident, type_ :: Type qualifiedName }
  | InstanceBindingName (ValueBindingFields qualifiedName)
-- | derive instance genericInstanceBinding :: Generic InstanceBinding _
-- | derive instance eqInstanceBinding :: Eq InstanceBinding
-- | derive instance ordInstanceBinding :: Ord InstanceBinding

type Instance qualifiedName =
  { head :: InstanceHead qualifiedName
  , body :: Array (InstanceBinding qualifiedName)
  }

infixl 5 ExprLambda as ====>
infixr 5 TypeArr as ====>>
infixr 5 KindArr as ====>>>
