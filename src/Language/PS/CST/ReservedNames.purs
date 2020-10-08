module Language.PS.CST.ReservedNames where

import Prelude
import Data.Set (Set)
import Data.Set as Set

reservedNames :: Set String
reservedNames = Set.fromFoldable
  [ "ado"
  , "case"
  , "class"
  , "data"
  , "derive"
  , "do"
  , "else"
  , "false"
  , "forall"
  , "foreign"
  , "import"
  , "if"
  , "in"
  , "infix"
  , "infixl"
  , "infixr"
  , "instance"
  , "let"
  , "module"
  , "newtype"
  , "of"
  , "true"
  , "type"
  , "where"
  ]

isReservedName :: String -> Boolean
isReservedName str = Set.member str reservedNames

appendUnderscoreIfReserved :: String -> String
appendUnderscoreIfReserved str =
  if isReservedName str
    then str <> "_"
    else str

quoteIfReserved :: String -> String
quoteIfReserved str =
  if isReservedName str
    then "\"" <> str <> "\""
    else str
