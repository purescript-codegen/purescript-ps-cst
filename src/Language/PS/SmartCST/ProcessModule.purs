module Language.PS.SmartCST.ProcessModule where

import Data.Tuple.Nested ((/\))
import Language.PS.CST.Types.Leafs (ModuleName)
import Language.PS.CST.Types.Module (Export, ImportDecl(..))
import Prelude

import Data.Array as Array
import Language.PS.SmartCST.Types.Declaration as SmartCST.Declaration
import Language.PS.SmartCST.ProcessSmartDeclaration as Language.PS.SmartCST.ProcessSmartDeclaration
import Text.Pretty as Text.Pretty
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

printModuleToString :: Int -> Module -> String
printModuleToString width = Text.Pretty.render width <<< Language.PS.CST.Printers.printModule <<< moduleToCstModule
