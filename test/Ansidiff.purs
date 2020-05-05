module Test.Ansidiff (ansidiffLines, textShouldMatch) where

import Prelude

import Ansi.Codes as Ansi.Codes
import Data.Function.Uncurried (Fn2, runFn2)
import Effect.Aff (Aff)
import Test.Spec.Assertions (fail)

reset :: String
reset = Ansi.Codes.escapeCodeToString (Ansi.Codes.Graphics (pure Ansi.Codes.Reset))

wrapInReset :: String → String
wrapInReset s = reset <> s <> reset

foreign import _ansidiffLines :: Fn2 String String String

ansidiffLines :: String -> String -> String
ansidiffLines actual expected = wrapInReset $ runFn2 _ansidiffLines actual expected

textShouldMatch :: String -> String -> Aff Unit
textShouldMatch actual expected = when (not $ eq actual expected) do
  fail $ "Text does not match\n\n" <> ansidiffLines actual expected
