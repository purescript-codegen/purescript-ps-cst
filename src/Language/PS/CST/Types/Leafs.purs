module Language.PS.CST.Types.Leafs where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable)

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
instance showComments :: Show Comments where show = genericShow

newtype ModuleName = ModuleName (NonEmptyArray (ProperName ProperNameType_Namespace))
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
  show (OpName string) = "(OpName " <> show string <> ")"

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
  show (ProperName string) = "(ProperName " <> show string <> ")"

data DeclDeriveType
  = DeclDeriveType_Newtype
  | DeclDeriveType_Ordinary
derive instance genericDeclDeriveType :: Generic DeclDeriveType _
derive instance eqDeclDeriveType :: Eq DeclDeriveType
derive instance ordDeclDeriveType :: Ord DeclDeriveType
instance showDeclDeriveType :: Show DeclDeriveType where show = genericShow

data Fixity
  = Infix
  | Infixl
  | Infixr
derive instance genericFixity :: Generic Fixity _
derive instance eqFixity :: Eq Fixity
derive instance ordFixity :: Ord Fixity
instance showFixity :: Show Fixity where show = genericShow

newtype Label = Label String
derive instance newtypeLabel :: Newtype Label _
derive instance genericLabel :: Generic Label _
derive instance eqLabel :: Eq Label
derive instance ordLabel :: Ord Label
instance showLabel :: Show Label where show = genericShow

data ClassFundep
  = FundepDetermines (NonEmptyArray Ident) (NonEmptyArray Ident)
  -- | FundepDetermined (NonEmptyArray Ident) -- parser is not allowing it (i.e. `class Foo a | a`)?

derive instance genericClassFundep :: Generic ClassFundep _
derive instance eqClassFundep :: Eq ClassFundep
derive instance ordClassFundep :: Ord ClassFundep
instance showClassFundep :: Show ClassFundep where show = genericShow

data RecordLabeled a
  = RecordPun Ident
  | RecordField Label a
derive instance functorRecordLabeled :: Functor RecordLabeled

instance foldableRecordLabeled :: Foldable RecordLabeled where
  foldMap f (RecordPun ident) = mempty
  foldMap f (RecordField label a) = f a
  foldl f x = foldlDefault f x
  foldr f x = foldrDefault f x

instance traversableRecordLabeled :: Traversable RecordLabeled where
  traverse _ (RecordPun ident)  = pure (RecordPun ident)
  traverse f (RecordField label x) = RecordField label <$> f x
  sequence (RecordPun ident)  = pure (RecordPun ident)
  sequence (RecordField label x) = RecordField label <$> x

derive instance genericRecordLabeled :: Generic (RecordLabeled a) _
derive instance eqRecordLabeled :: Eq a => Eq (RecordLabeled a)
derive instance ordRecordLabeled :: Ord a => Ord (RecordLabeled a)
instance showRecordLabeled :: Show a => Show (RecordLabeled a) where
  show (RecordPun ident) = "(RecordPun " <> show ident <> ")"
  show (RecordField label a) = "(RecordField { label = " <> show label <> ", a = " <> show a <> " })"
