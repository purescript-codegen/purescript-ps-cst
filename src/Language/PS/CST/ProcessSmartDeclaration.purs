module Language.PS.CST.ProcessSmartDeclaration where

import Data.Tuple.Nested
import Language.PS.CST.Types.Module
import Language.PS.CST.Types.QualifiedName
import Language.PS.CST.Types.Shared
import Language.PS.CST.Types.SmartQualifiedName
import Prelude

import Control.Monad.State (State, modify_)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Maybe (fromMaybe) as Maybe
import Unsafe.Coerce (unsafeCoerce)

type App = State (Array ImportDecl)

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

---------------------------------

processDeclaration :: Declaration SmartQualifiedName -> Declaration QualifiedName
processDeclaration (DeclData          { comments, head, constructors }) = DeclData { comments, head: processDataHead head, constructors: map processDataCtor constructors }
processDeclaration (DeclType          { comments, head, type_ }) = unsafeCoerce unit
processDeclaration (DeclNewtype       { comments, head, name }) = unsafeCoerce unit
processDeclaration (DeclClass         { comments, head, methods }) = unsafeCoerce unit
processDeclaration (DeclInstanceChain { comments, instances }) = unsafeCoerce unit
processDeclaration (DeclDerive        { comments, deriveType, head }) = unsafeCoerce unit
processDeclaration (DeclSignature     { comments, ident, type_ }) = unsafeCoerce unit
processDeclaration (DeclValue         { comments, valueBindingFields }) = unsafeCoerce unit
processDeclaration (DeclFixity        { comments, fixityFields }) = unsafeCoerce unit
processDeclaration (DeclForeign       { comments, foreign_ }) = unsafeCoerce unit

processDataHead :: DataHead SmartQualifiedName -> DataHead QualifiedName
processDataHead (DataHead { dataHdName, dataHdVars }) = DataHead { dataHdName, dataHdVars: map processTypeVarBinding dataHdVars }

processKind :: Kind SmartQualifiedName -> Kind QualifiedName
processKind (KindName prop) = KindName (processSmartQualifiedName prop)
processKind (KindArr kindA kindB) = KindArr (processKind kindA) (processKind kindB)
processKind (KindRow kindA) = KindRow (processKind kindA)

processSmartQualifiedName :: SmartQualifiedName (ProperName ProperNameType_KindName) -> App (QualifiedName (ProperName ProperNameType_KindName))
processSmartQualifiedName (SmartQualifiedName { module_, importType, name }) =
  case importType of
       SmartQualifiedNameImportType__Full -> do
          modify_ $ findAndModifyOrNew
            (\(ImportDecl import_) ->
              case import_.qualification of
                   Nothing -> false
                   Just moduleName -> module_ == moduleName
            )
            (\(ImportDecl import_) ->
              ImportDecl $ import_
                { names =
                  findOrNew
                  (case _ of
                        ImportKind name' -> name' == name
                        _ -> false
                  )
                  (\_ -> ImportKind name)
                  import_.names
                }
            )
            (\_ -> ImportDecl
              { moduleName: module_
              , names: [ImportKind name]
              , qualification: Just module_
              }
            )
          pure $ QualifiedName { qualModule: Just module_, qualName: name }
       SmartQualifiedNameImportType__None -> do
          pure $ QualifiedName { qualModule: Nothing, qualName: name }
       (SmartQualifiedNameImportType__Custom customModule) -> do
          pure $ QualifiedName { qualModule: Just customModule, qualName: name }

processTypeVarBinding :: TypeVarBinding SmartQualifiedName -> TypeVarBinding QualifiedName
processTypeVarBinding (TypeVarKinded ident kind_) = TypeVarKinded ident (processKind kind_)
processTypeVarBinding (TypeVarName ident) = TypeVarName ident

processDataCtor :: DataCtor SmartQualifiedName -> DataCtor QualifiedName
processDataCtor (DataCtor { dataCtorName, dataCtorFields }) = unsafeCoerce unit

processType :: Type SmartQualifiedName -> Type QualifiedName
processType _ = unsafeCoerce unit

processInstanceHead :: InstanceHead SmartQualifiedName -> InstanceHead QualifiedName
processInstanceHead _ = unsafeCoerce unit

processForeign :: Foreign SmartQualifiedName -> Foreign QualifiedName
processForeign _ = unsafeCoerce unit

processFixityFields :: FixityFields SmartQualifiedName -> FixityFields QualifiedName
processFixityFields _ = unsafeCoerce unit

processFixityOp :: FixityOp SmartQualifiedName -> FixityOp QualifiedName
processFixityOp _ = unsafeCoerce unit

processRow :: Row SmartQualifiedName -> Row QualifiedName
processRow _ = unsafeCoerce unit

processConstraint :: Constraint SmartQualifiedName -> Constraint QualifiedName
processConstraint _ = unsafeCoerce unit

processClassHead :: ClassHead SmartQualifiedName -> ClassHead QualifiedName
processClassHead _ = unsafeCoerce unit

processValueBindingFields :: ValueBindingFields SmartQualifiedName -> ValueBindingFields QualifiedName
processValueBindingFields _ = unsafeCoerce unit

processBinder :: Binder SmartQualifiedName -> Binder QualifiedName
processBinder _ = unsafeCoerce unit

processGuarded :: Guarded SmartQualifiedName -> Guarded QualifiedName
processGuarded _ = unsafeCoerce unit

processWhere :: Where SmartQualifiedName -> Where QualifiedName
processWhere _ = unsafeCoerce unit

processLetBinding :: LetBinding SmartQualifiedName -> LetBinding QualifiedName
processLetBinding _ = unsafeCoerce unit

processGuardedExpr :: GuardedExpr SmartQualifiedName -> GuardedExpr QualifiedName
processGuardedExpr _ = unsafeCoerce unit

processPatternGuard :: PatternGuard SmartQualifiedName -> PatternGuard QualifiedName
processPatternGuard _ = unsafeCoerce unit

processExpr :: Expr SmartQualifiedName -> Expr QualifiedName
processExpr _ = unsafeCoerce unit

processRecordAccessor :: RecordAccessor SmartQualifiedName -> RecordAccessor QualifiedName
processRecordAccessor _ = unsafeCoerce unit

processRecordUpdate :: RecordUpdate SmartQualifiedName -> RecordUpdate QualifiedName
processRecordUpdate _ = unsafeCoerce unit

processLambda :: Lambda SmartQualifiedName -> Lambda QualifiedName
processLambda _ = unsafeCoerce unit

processIfThenElse :: IfThenElse SmartQualifiedName -> IfThenElse QualifiedName
processIfThenElse _ = unsafeCoerce unit

processCaseOf :: CaseOf SmartQualifiedName -> CaseOf QualifiedName
processCaseOf _ = unsafeCoerce unit

processLetIn :: LetIn SmartQualifiedName -> LetIn QualifiedName
processLetIn _ = unsafeCoerce unit

processDoStatement :: DoStatement SmartQualifiedName -> DoStatement QualifiedName
processDoStatement _ = unsafeCoerce unit

processAdoBlock :: AdoBlock SmartQualifiedName -> AdoBlock QualifiedName
processAdoBlock _ = unsafeCoerce unit

processInstanceBinding :: InstanceBinding SmartQualifiedName -> InstanceBinding QualifiedName
processInstanceBinding _ = unsafeCoerce unit

processInstance :: Instance SmartQualifiedName -> Instance QualifiedName
processInstance _ = unsafeCoerce unit
