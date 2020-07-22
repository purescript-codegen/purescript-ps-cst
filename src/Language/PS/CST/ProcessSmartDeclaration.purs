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
import Data.Traversable (traverse)
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

processDeclaration :: Declaration SmartQualifiedName -> App (Declaration QualifiedName)
processDeclaration (DeclData          { comments, head, constructors }) = do
  (head' :: DataHead QualifiedName) <- processDataHead head
  (constructors' :: Array (DataCtor QualifiedName)) <- traverse processDataCtor constructors
  pure $ DeclData { comments, head: head', constructors: constructors' }
processDeclaration (DeclType          { comments, head, type_ }) = unsafeCoerce unit
processDeclaration (DeclNewtype       { comments, head, name }) = unsafeCoerce unit
processDeclaration (DeclClass         { comments, head, methods }) = unsafeCoerce unit
processDeclaration (DeclInstanceChain { comments, instances }) = unsafeCoerce unit
processDeclaration (DeclDerive        { comments, deriveType, head }) = unsafeCoerce unit
processDeclaration (DeclSignature     { comments, ident, type_ }) = unsafeCoerce unit
processDeclaration (DeclValue         { comments, valueBindingFields }) = unsafeCoerce unit
processDeclaration (DeclFixity        { comments, fixityFields }) = unsafeCoerce unit
processDeclaration (DeclForeign       { comments, foreign_ }) = unsafeCoerce unit

processDataHead :: DataHead SmartQualifiedName -> App (DataHead QualifiedName)
processDataHead (DataHead { dataHdName, dataHdVars }) = do
  (dataHdVars' :: Array (TypeVarBinding QualifiedName)) <- traverse processTypeVarBinding dataHdVars
  pure $ DataHead { dataHdName, dataHdVars: dataHdVars' }

processKind :: Kind SmartQualifiedName -> App (Kind QualifiedName)
processKind (KindName prop) = KindName <$> processSmartQualifiedName prop
processKind (KindArr kindA kindB) = KindArr <$> (processKind kindA) <*> (processKind kindB)
processKind (KindRow kindA) = KindRow <$> (processKind kindA)

processSmartQualifiedName :: SmartQualifiedName (ProperName ProperNameType_KindName) -> App (QualifiedName (ProperName ProperNameType_KindName))
processSmartQualifiedName (SmartQualifiedName smartQualifiedName) =
  case smartQualifiedName.importType of
       SmartQualifiedNameImportType__Full -> do
          modify_ $ findAndModifyOrNew
            (\(ImportDecl import_) ->
              case import_.qualification of
                   Nothing -> false
                   Just moduleName -> smartQualifiedName.module_ == moduleName
            )
            (\(ImportDecl import_) ->
              ImportDecl $ import_
                { names =
                  findOrNew
                  (case _ of
                        ImportKind name' -> name' == smartQualifiedName.name
                        _ -> false
                  )
                  (\_ -> ImportKind smartQualifiedName.name)
                  import_.names
                }
            )
            (\_ -> ImportDecl
              { moduleName: smartQualifiedName.module_
              , names: [ImportKind smartQualifiedName.name]
              , qualification: Just smartQualifiedName.module_
              }
            )
          pure $ QualifiedName { qualModule: Just smartQualifiedName.module_, qualName: smartQualifiedName.name }
       SmartQualifiedNameImportType__None -> do
          modify_ $ findAndModifyOrNew
            (\(ImportDecl import_) ->
              case import_.qualification of
                   Nothing -> import_.moduleName == smartQualifiedName.module_
                   Just _ -> false
            )
            (\(ImportDecl import_) ->
              ImportDecl $ import_
                { names =
                  findOrNew
                  (case _ of
                        ImportKind name' -> name' == smartQualifiedName.name
                        _ -> false
                  )
                  (\_ -> ImportKind smartQualifiedName.name)
                  import_.names
                }
            )
            (\_ -> ImportDecl
              { moduleName: smartQualifiedName.module_
              , names: [ImportKind smartQualifiedName.name]
              , qualification: Nothing
              }
            )
          pure $ QualifiedName { qualModule: Nothing, qualName: smartQualifiedName.name }
       (SmartQualifiedNameImportType__Custom customModule) -> do
          modify_ $ findAndModifyOrNew
            (\(ImportDecl import_) ->
              case import_.qualification of
                   Nothing -> false
                   Just moduleName -> moduleName == customModule
            )
            (\(ImportDecl import_) ->
              ImportDecl $ import_
                { names =
                  findOrNew
                  (case _ of
                        ImportKind name' -> name' == smartQualifiedName.name
                        _ -> false
                  )
                  (\_ -> ImportKind smartQualifiedName.name)
                  import_.names
                }
            )
            (\_ -> ImportDecl
              { moduleName: smartQualifiedName.module_
              , names: [ImportKind smartQualifiedName.name]
              , qualification: Just customModule
              }
            )
          pure $ QualifiedName { qualModule: Just customModule, qualName: smartQualifiedName.name }

processTypeVarBinding :: TypeVarBinding SmartQualifiedName -> App (TypeVarBinding QualifiedName)
processTypeVarBinding (TypeVarKinded ident kind_) = TypeVarKinded ident <$> (processKind kind_)
processTypeVarBinding (TypeVarName ident) = pure $ TypeVarName ident

processDataCtor :: DataCtor SmartQualifiedName -> App (DataCtor QualifiedName)
processDataCtor (DataCtor { dataCtorName, dataCtorFields }) = unsafeCoerce unit

processType :: Type SmartQualifiedName -> App (Type QualifiedName)
processType _ = unsafeCoerce unit

processInstanceHead :: InstanceHead SmartQualifiedName -> App (InstanceHead QualifiedName)
processInstanceHead _ = unsafeCoerce unit

processForeign :: Foreign SmartQualifiedName -> App (Foreign QualifiedName)
processForeign _ = unsafeCoerce unit

processFixityFields :: FixityFields SmartQualifiedName -> App (FixityFields QualifiedName)
processFixityFields _ = unsafeCoerce unit

processFixityOp :: FixityOp SmartQualifiedName -> App (FixityOp QualifiedName)
processFixityOp _ = unsafeCoerce unit

processRow :: Row SmartQualifiedName -> App (Row QualifiedName)
processRow _ = unsafeCoerce unit

processConstraint :: Constraint SmartQualifiedName -> App (Constraint QualifiedName)
processConstraint _ = unsafeCoerce unit

processClassHead :: ClassHead SmartQualifiedName -> App (ClassHead QualifiedName)
processClassHead _ = unsafeCoerce unit

processValueBindingFields :: ValueBindingFields SmartQualifiedName -> App (ValueBindingFields QualifiedName)
processValueBindingFields _ = unsafeCoerce unit

processBinder :: Binder SmartQualifiedName -> App (Binder QualifiedName)
processBinder _ = unsafeCoerce unit

processGuarded :: Guarded SmartQualifiedName -> App (Guarded QualifiedName)
processGuarded _ = unsafeCoerce unit

processWhere :: Where SmartQualifiedName -> App (Where QualifiedName)
processWhere _ = unsafeCoerce unit

processLetBinding :: LetBinding SmartQualifiedName -> App (LetBinding QualifiedName)
processLetBinding _ = unsafeCoerce unit

processGuardedExpr :: GuardedExpr SmartQualifiedName -> App (GuardedExpr QualifiedName)
processGuardedExpr _ = unsafeCoerce unit

processPatternGuard :: PatternGuard SmartQualifiedName -> App (PatternGuard QualifiedName)
processPatternGuard _ = unsafeCoerce unit

processExpr :: Expr SmartQualifiedName -> App (Expr QualifiedName)
processExpr _ = unsafeCoerce unit

processRecordAccessor :: RecordAccessor SmartQualifiedName -> App (RecordAccessor QualifiedName)
processRecordAccessor _ = unsafeCoerce unit

processRecordUpdate :: RecordUpdate SmartQualifiedName -> App (RecordUpdate QualifiedName)
processRecordUpdate _ = unsafeCoerce unit

processLambda :: Lambda SmartQualifiedName -> App (Lambda QualifiedName)
processLambda _ = unsafeCoerce unit

processIfThenElse :: IfThenElse SmartQualifiedName -> App (IfThenElse QualifiedName)
processIfThenElse _ = unsafeCoerce unit

processCaseOf :: CaseOf SmartQualifiedName -> App (CaseOf QualifiedName)
processCaseOf _ = unsafeCoerce unit

processLetIn :: LetIn SmartQualifiedName -> App (LetIn QualifiedName)
processLetIn _ = unsafeCoerce unit

processDoStatement :: DoStatement SmartQualifiedName -> App (DoStatement QualifiedName)
processDoStatement _ = unsafeCoerce unit

processAdoBlock :: AdoBlock SmartQualifiedName -> App (AdoBlock QualifiedName)
processAdoBlock _ = unsafeCoerce unit

processInstanceBinding :: InstanceBinding SmartQualifiedName -> App (InstanceBinding QualifiedName)
processInstanceBinding _ = unsafeCoerce unit

processInstance :: Instance SmartQualifiedName -> App (Instance QualifiedName)
processInstance _ = unsafeCoerce unit
