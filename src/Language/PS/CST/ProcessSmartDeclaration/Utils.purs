module Language.PS.CST.ProcessSmartDeclaration.Utils where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Maybe (fromMaybe) as Maybe

findAndModifyOrNew :: forall a . (a -> Boolean) -> (a -> a) -> (Unit -> a) -> Array a -> Array a
findAndModifyOrNew find modify new array =
  case Array.findIndex find array of
       Nothing -> Array.snoc array (new unit)
       Just index -> Array.modifyAt index modify array # Maybe.fromMaybe array

findOrNew :: forall a . (a -> Boolean) -> (Unit -> a) -> Array a -> Array a
findOrNew find new array =
  case Array.findIndex find array of
       Nothing -> Array.snoc array (new unit)
       Just index -> array

