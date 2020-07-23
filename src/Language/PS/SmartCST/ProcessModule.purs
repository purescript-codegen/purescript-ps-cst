module Language.PS.SmartCST.ProcessModule where

import Data.Tuple.Nested
import Language.PS.CST.Types.Leafs
import Language.PS.CST.Types.Module
import Language.PS.CST.Types.QualifiedName
import Language.PS.SmartCST.ProcessSmartDeclaration
import Language.PS.SmartCST.Types.ConstructorProperName
import Language.PS.SmartCST.Types.SmartQualifiedName
import Prelude

import Control.Monad.State (State, modify_, runState)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Traversable (traverse)
import Language.PS.SmartCST.Types.Declaration as SmartCST.Declaration
import Language.PS.SmartCST.ProcessSmartDeclaration as Language.PS.SmartCST.ProcessSmartDeclaration
import Text.PrettyPrint.Boxes (render) as Text.PrettyPrint.Boxes
import Language.PS.CST.Printers as Language.PS.CST.Printers
import Language.PS.CST.Types.Module as Language.PS.CST.Types.Module

newtype Module = Module
  { moduleName :: ModuleName
  , exports :: Array Export
  , declarations :: Array SmartCST.Declaration.Declaration
  }

moduleToCstModule :: Module -> Language.PS.CST.Types.Module.Module
moduleToCstModule (Module module_) =
  let
    (declarations /\ imports) = Language.PS.SmartCST.ProcessSmartDeclaration.processDeclarations module_.moduleName module_.declarations

    importsWithoutCurrentModule = Array.filter (\(ImportDecl importDecl) -> importDecl.moduleName /= module_.moduleName) imports
  in
    Language.PS.CST.Types.Module.Module
    { moduleName: module_.moduleName
    , imports: importsWithoutCurrentModule
    , exports: module_.exports
    , declarations
    }

printModuleToString :: Module -> String
printModuleToString = Text.PrettyPrint.Boxes.render <<< Language.PS.CST.Printers.printModule <<< moduleToCstModule
