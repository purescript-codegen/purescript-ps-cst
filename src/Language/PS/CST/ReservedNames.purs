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

appendUnderscoreIfReserved :: String -> String
appendUnderscoreIfReserved str =
  if Set.member str reservedNames
    then str <> "_"
    else str

quoteIfReserved :: String -> String
quoteIfReserved str =
  if Set.member str reservedNames
    then "\"" <> str <> "\""
    else str
