module Test.Main where

import Effect
import Effect.Aff
import Language.PS.AST
import Prelude

import Control.Parallel (parTraverse)
import Data.Either (Either(..))
import Data.Functor.Mu (roll)
import Data.List (fromFoldable) as List
import Data.Maybe (Maybe(..))
import Data.String.Regex as Regex
import Data.String.Regex.Unsafe as Regex
import Data.String.Regex.Flags as Regex
import Data.Traversable (traverse_)
import Effect.Class (liftEffect)
import Language.PS.AST.Printers as Language.PS.AST.Printers
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Path as Node.Path
import Test.Ansidiff (textShouldMatch)
import Test.Golden.WithEnum.Actual as Test.Golden.WithEnum.Actual
import Test.Golden.WithEnumWithRecord.Actual as Test.Golden.WithEnumWithRecord.Actual
import Test.Spec as Test.Spec
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter as Test.Spec.Reporter
import Test.Spec.Runner as Test.Spec.Runner
import Text.PrettyPrint.Boxes (render) as Text.PrettyPrint.Boxes

type GoldenTest =
  { name :: String
  , actualModule :: Module
  }

type GoldenTestWithExpected =
  { name :: String
  , actualModule :: Module
  , expected :: String
  }

goldenTests :: Array GoldenTest
goldenTests =
  [ { name: "WithEnum", actualModule: Test.Golden.WithEnum.Actual.actualModule }
  , { name: "WithEnumWithRecord", actualModule: Test.Golden.WithEnumWithRecord.Actual.actualModule }
  ]

addText :: GoldenTest -> Aff GoldenTestWithExpected
addText test = do
  let path = Node.Path.concat ["test", "Golden", test.name, "Expected.txt"]
  absolutePath <- liftEffect $ Node.Path.resolve [] path
  expected <- readTextFile UTF8 absolutePath
  pure $ { name: test.name, actualModule: test.actualModule, expected }

mkAllTests :: Array GoldenTestWithExpected -> Test.Spec.Spec Unit
mkAllTests tests = traverse_ mkTest tests
  where
  mkTest :: GoldenTestWithExpected -> Test.Spec.Spec Unit
  mkTest test = Test.Spec.it test.name do
    let
      actualParsed = Text.PrettyPrint.Boxes.render $ Language.PS.AST.Printers.printModule test.actualModule
    actualParsed `textShouldMatch` test.expected

main :: Effect Unit
main = launchAff_ do
  (goldenTestsWithExpected :: Array GoldenTestWithExpected) <- flip parTraverse goldenTests addText

  let
    (allTests :: Test.Spec.Spec Unit) = mkAllTests goldenTestsWithExpected
  Test.Spec.Runner.runSpec' Test.Spec.Runner.defaultConfig [ Test.Spec.Reporter.consoleReporter ] allTests
