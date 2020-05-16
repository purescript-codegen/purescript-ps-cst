module Test.Main where

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Language.PS.CST (Module)
import Prelude (Unit, bind, flip, pure, ($))

import Control.Parallel (parTraverse)
import Data.Traversable (traverse_)
import Effect.Class (liftEffect)
import Language.PS.CST.Printers as Language.PS.CST.Printers
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Path as Node.Path
import Test.Ansidiff (textShouldMatch)
import Test.Spec as Test.Spec
import Test.Spec.Reporter as Test.Spec.Reporter
import Test.Spec.Runner as Test.Spec.Runner

import Test.Golden.DeclType.Actual                               as Test.Golden.DeclType.Actual
import Test.Golden.DeclNewtype.Actual                            as Test.Golden.DeclNewtype.Actual
import Test.Golden.DeclData.Actual                               as Test.Golden.DeclData.Actual
import Test.Golden.DeclDataComplex.Actual                        as Test.Golden.DeclDataComplex.Actual
import Test.Golden.DeclFixity.Actual                             as Test.Golden.DeclFixity.Actual
import Test.Golden.DeclForeign.Actual                            as Test.Golden.DeclForeign.Actual
import Test.Golden.DeclDerive.Actual                             as Test.Golden.DeclDerive.Actual
import Test.Golden.DeclClass.Actual                              as Test.Golden.DeclClass.Actual
import Test.Golden.Imports.Actual                                as Test.Golden.Imports.Actual
import Test.Golden.Exports.Actual                                as Test.Golden.Exports.Actual
import Test.Golden.Boolean.Actual                                as Test.Golden.Boolean.Actual
import Test.Golden.Application.Actual                            as Test.Golden.Application.Actual
import Test.Golden.MultilinePatternMatchingInLet.Actual          as Test.Golden.MultilinePatternMatchingInLet.Actual
import Test.Golden.MultilinePatternMatchingInLet2.Actual         as Test.Golden.MultilinePatternMatchingInLet2.Actual
import Test.Golden.MultilinePatternMatchingInWhere.Actual        as Test.Golden.MultilinePatternMatchingInWhere.Actual
import Test.Golden.MultilinePatternMatchingInWhere2.Actual       as Test.Golden.MultilinePatternMatchingInWhere2.Actual
import Test.Golden.MultilinePatternMatchingInWhereAndLet2.Actual as Test.Golden.MultilinePatternMatchingInWhereAndLet2.Actual
import Test.Golden.Case.Actual                                   as Test.Golden.Case.Actual
import Test.Golden.If.Actual                                     as Test.Golden.If.Actual
import Test.Golden.Instance.Actual                               as Test.Golden.Instance.Actual
import Test.Golden.InstanceChain.Actual                          as Test.Golden.InstanceChain.Actual

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
  , { name: "DeclClass", actualModule: Test.Golden.DeclClass.Actual.actualModule }
  , { name: "Boolean", actualModule: Test.Golden.Boolean.Actual.actualModule }
  , { name: "Application", actualModule: Test.Golden.Application.Actual.actualModule }
  , { name: "MultilinePatternMatchingInLet", actualModule: Test.Golden.MultilinePatternMatchingInLet.Actual.actualModule }
  , { name: "MultilinePatternMatchingInLet2", actualModule: Test.Golden.MultilinePatternMatchingInLet2.Actual.actualModule }
  , { name: "MultilinePatternMatchingInWhere", actualModule: Test.Golden.MultilinePatternMatchingInWhere.Actual.actualModule }
  , { name: "MultilinePatternMatchingInWhere2", actualModule: Test.Golden.MultilinePatternMatchingInWhere2.Actual.actualModule }
  , { name: "MultilinePatternMatchingInWhereAndLet2", actualModule: Test.Golden.MultilinePatternMatchingInWhereAndLet2.Actual.actualModule }
  , { name: "Case", actualModule: Test.Golden.Case.Actual.actualModule }
  , { name: "If", actualModule: Test.Golden.If.Actual.actualModule }
  , { name: "Instance", actualModule: Test.Golden.Instance.Actual.actualModule }
  , { name: "InstanceChain", actualModule: Test.Golden.InstanceChain.Actual.actualModule }
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
      actualParsed = Language.PS.CST.Printers.printModuleToString test.actualModule
    actualParsed `textShouldMatch` test.expected

main :: Effect Unit
main = launchAff_ do
  (goldenTestsWithExpected :: Array GoldenTestWithExpected) <- flip parTraverse goldenTests addText

  let
    (allTests :: Test.Spec.Spec Unit) = mkAllTests goldenTestsWithExpected
  Test.Spec.Runner.runSpec' Test.Spec.Runner.defaultConfig [ Test.Spec.Reporter.consoleReporter ] allTests
