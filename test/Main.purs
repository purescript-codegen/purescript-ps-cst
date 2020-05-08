module Test.Main where

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Language.PS.AST (Module)
import Prelude (Unit, bind, flip, pure, ($))

import Control.Parallel (parTraverse)
import Data.Traversable (traverse_)
import Effect.Class (liftEffect)
import Language.PS.AST.Printers as Language.PS.AST.Printers
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Path as Node.Path
import Test.Ansidiff (textShouldMatch)
import Test.Spec as Test.Spec
import Test.Spec.Reporter as Test.Spec.Reporter
import Test.Spec.Runner as Test.Spec.Runner
import Text.PrettyPrint.Boxes (render) as Text.PrettyPrint.Boxes

import Test.Golden.DeclType.Actual        as Test.Golden.DeclType.Actual
import Test.Golden.DeclNewtype.Actual     as Test.Golden.DeclNewtype.Actual
import Test.Golden.DeclData.Actual        as Test.Golden.DeclData.Actual
import Test.Golden.DeclDataComplex.Actual as Test.Golden.DeclDataComplex.Actual
import Test.Golden.DeclFixity.Actual      as Test.Golden.DeclFixity.Actual
import Test.Golden.DeclForeign.Actual     as Test.Golden.DeclForeign.Actual
import Test.Golden.DeclDerive.Actual      as Test.Golden.DeclDerive.Actual
import Test.Golden.Imports.Actual         as Test.Golden.Imports.Actual
import Test.Golden.Exports.Actual         as Test.Golden.Exports.Actual

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
  [ { name: "Imports", actualModule: Test.Golden.Imports.Actual.actualModule }
  , { name: "Exports", actualModule: Test.Golden.Exports.Actual.actualModule }
  , { name: "DeclData", actualModule: Test.Golden.DeclData.Actual.actualModule }
  , { name: "DeclDataComplex", actualModule: Test.Golden.DeclDataComplex.Actual.actualModule }
  , { name: "DeclType", actualModule: Test.Golden.DeclType.Actual.actualModule }
  , { name: "DeclNewtype", actualModule: Test.Golden.DeclNewtype.Actual.actualModule }
  , { name: "DeclFixity", actualModule: Test.Golden.DeclFixity.Actual.actualModule }
  , { name: "DeclForeign", actualModule: Test.Golden.DeclForeign.Actual.actualModule }
  , { name: "DeclDerive", actualModule: Test.Golden.DeclDerive.Actual.actualModule }
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
