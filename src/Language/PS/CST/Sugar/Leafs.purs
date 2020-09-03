module Language.PS.CST.Sugar.Leafs where

import Language.PS.CST.Types.Leafs (Label(..), ModuleName(..), ProperName(..))
import Prelude

import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Tuple.Nested (type (/\), (/\))

mkModuleName :: NonEmptyArray String -> ModuleName
mkModuleName = ModuleName <<< map ProperName

emptyRow :: forall labelType type_ . { rowLabels :: Array labelType, rowTail :: Maybe type_ }
emptyRow = { rowLabels: [], rowTail: Nothing }

mkRowLabels :: forall type_ . Array (String /\ type_) -> Array { label :: Label, type_ :: type_ }
mkRowLabels = map mkRowLabel

mkRowLabel :: forall type_ . (String /\ type_) -> { label :: Label, type_ :: type_ }
mkRowLabel = (\(label /\ type_) -> { label: Label label, type_ })
