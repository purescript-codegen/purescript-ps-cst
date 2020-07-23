module Language.PS.SmartCST.ProcessSmartDeclaration where

import Data.Tuple.Nested
import Language.PS.SmartCST.ProcessSmartDeclaration.Utils
import Language.PS.CST.Types.Module
import Language.PS.CST.Types.QualifiedName
import Language.PS.CST.Types.Declaration
import Language.PS.CST.Types.Leafs
import Language.PS.SmartCST.Types.SmartQualifiedName
import Prelude

import Control.Monad.State (State, modify_)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe (fromMaybe) as Maybe
import Data.Traversable (traverse)
import Unsafe.Coerce (unsafeCoerce)

type App = State (Array ImportDecl)

---------------------------------

-- Generalized
processSmartQualifiedName
  :: forall a
   . (a -> Import -> Boolean)
  -> (a -> Import -> Import)
  -> (a -> Import)
  -> SmartQualifiedName a
  -> App (QualifiedName a)
processSmartQualifiedName
  smartFindEqToName
  smartModify
  smartCreateNew
  (SmartQualifiedName smartQualifiedName) =
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
                  findAndModifyOrNew
                  (smartFindEqToName smartQualifiedName.name)
                  (smartModify smartQualifiedName.name)
                  (\_ -> smartCreateNew smartQualifiedName.name)
                  import_.names
                }
            )
            (\_ -> ImportDecl
              { moduleName: smartQualifiedName.module_
              , names: [smartCreateNew smartQualifiedName.name]
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
                  findAndModifyOrNew
                  (smartFindEqToName smartQualifiedName.name)
                  (smartModify smartQualifiedName.name)
                  (\_ -> smartCreateNew smartQualifiedName.name)
                  import_.names
                }
            )
            (\_ -> ImportDecl
              { moduleName: smartQualifiedName.module_
              , names: [smartCreateNew smartQualifiedName.name]
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
                  findAndModifyOrNew
                  (smartFindEqToName smartQualifiedName.name)
                  (smartModify smartQualifiedName.name)
                  (\_ -> smartCreateNew smartQualifiedName.name)
                  import_.names
                }
            )
            (\_ -> ImportDecl
              { moduleName: smartQualifiedName.module_
              , names: [smartCreateNew smartQualifiedName.name]
              , qualification: Just customModule
              }
            )
          pure $ QualifiedName { qualModule: Just customModule, qualName: smartQualifiedName.name }

-- For each 6 of Import constructors (plus processSmartQualifiedNameTypeConstructor)
processSmartQualifiedNameValue :: SmartQualifiedName Ident -> App (QualifiedName Ident)
processSmartQualifiedNameValue = processSmartQualifiedName
  (\expectedName ->
    case _ of
      ImportValue name' -> name' == expectedName
      _ -> false
  )
  (\_name currentImport -> currentImport)
  ImportValue

processSmartQualifiedNameOp :: SmartQualifiedName (OpName OpNameType_ValueOpName) -> App (QualifiedName (OpName OpNameType_ValueOpName))
processSmartQualifiedNameOp = processSmartQualifiedName
  (\expectedName ->
    case _ of
      ImportOp name' -> name' == expectedName
      _ -> false
  )
  (\name currentImport -> currentImport)
  ImportOp

-- adds `import Module.Name (Foo)`
processSmartQualifiedNameType :: SmartQualifiedName (ProperName ProperNameType_TypeName) -> App (QualifiedName (ProperName ProperNameType_TypeName))
processSmartQualifiedNameType = processSmartQualifiedName
  (\expectedName ->
    case _ of
      ImportType name' _ -> name' == expectedName
      _ -> false
  )
  (\name currentImport -> currentImport) -- if there is `import Module.Name (Foo(..))` - leave it as it is
  (\name -> ImportType name Nothing) -- but if it is new - create without constructors `import Module.Name (Foo)`

-- is like processSmartQualifiedNameType, but adds -- adds `import Module.Name (Foo(..))`
processSmartQualifiedNameTypeConstructor :: SmartQualifiedName (ProperName ProperNameType_ConstructorName) -> App (QualifiedName (ProperName ProperNameType_ConstructorName))
processSmartQualifiedNameTypeConstructor = processSmartQualifiedName
  (\expectedName ->
    case _ of
      ImportType name' _ -> name' == expectedName
      _ -> false
  )
  (\name _currentImport -> ImportType name (Just DataAll))
  (\name -> ImportType name (Just DataAll))

processSmartQualifiedNameTypeOp :: SmartQualifiedName (OpName OpNameType_TypeOpName) -> App (QualifiedName (OpName OpNameType_TypeOpName))
processSmartQualifiedNameTypeOp = processSmartQualifiedName
  (\expectedName ->
    case _ of
      ImportTypeOp name' -> name' == expectedName
      _ -> false
  )
  (\name currentImport -> currentImport)
  ImportTypeOp

processSmartQualifiedNameClass :: SmartQualifiedName (ProperName ProperNameType_ClassName) -> App (QualifiedName (ProperName ProperNameType_ClassName))
processSmartQualifiedNameClass = processSmartQualifiedName
  (\expectedName ->
    case _ of
      ImportClass name' -> name' == expectedName
      _ -> false
  )
  (\name currentImport -> currentImport)
  ImportClass

processSmartQualifiedNameKind :: SmartQualifiedName (ProperName ProperNameType_KindName) -> App (QualifiedName (ProperName ProperNameType_KindName))
processSmartQualifiedNameKind = processSmartQualifiedName
  (\expectedName ->
    case _ of
      ImportKind name' -> name' == expectedName
      _ -> false
  )
  (\name currentImport -> currentImport)
  ImportKind

---------------------------------

processDeclaration :: Declaration SmartQualifiedName -> App (Declaration QualifiedName)
processDeclaration (DeclData { comments, head, constructors }) = do
  (head' :: DataHead QualifiedName) <- processDataHead head
  (constructors' :: Array (DataCtor QualifiedName)) <- traverse processDataCtor constructors
  pure $ DeclData { comments, head: head', constructors: constructors' }
processDeclaration (DeclType { comments, head, type_ }) = do
  (head' :: DataHead QualifiedName) <- processDataHead head
  (type_' :: Type QualifiedName) <- processType type_
  pure $ DeclType { comments, head: head', type_: type_' }
processDeclaration (DeclNewtype { comments, head, name, type_ }) = do
  (head' :: DataHead QualifiedName) <- processDataHead head
  (type_' :: Type QualifiedName) <- processType type_
  pure $ DeclNewtype { comments, head: head', name, type_: type_' }
processDeclaration (DeclClass { comments, head, methods }) = do
  (head' :: ClassHead QualifiedName) <- processClassHead head
  (methods' :: Array { ident :: Ident, type_ :: Type QualifiedName }) <- traverse (\{ ident, type_ } -> { ident, type_: _ } <$> processType type_) methods
  pure $ DeclClass { comments, head: head', methods: methods' }
processDeclaration (DeclInstanceChain { comments, instances }) = do
  (instances' :: NonEmptyArray (Instance QualifiedName)) <- traverse processInstance instances
  pure $ DeclInstanceChain { comments, instances: instances' }
processDeclaration (DeclDerive { comments, deriveType, head }) = do
  (head' :: InstanceHead QualifiedName) <- processInstanceHead head
  pure $ DeclDerive { comments, deriveType, head: head' }
processDeclaration (DeclSignature { comments, ident, type_ }) = do
  (type_' :: Type QualifiedName) <- processType type_
  pure $ DeclSignature { comments, ident, type_: type_' }
processDeclaration (DeclValue { comments, valueBindingFields }) = do
  (valueBindingFields' :: ValueBindingFields QualifiedName) <- processValueBindingFields valueBindingFields
  pure $ DeclValue { comments, valueBindingFields: valueBindingFields' }
processDeclaration (DeclFixity { comments, fixityFields }) = do
  (fixityFields' :: FixityFields QualifiedName) <- processFixityFields fixityFields
  pure $ DeclFixity { comments, fixityFields: fixityFields' }
processDeclaration (DeclForeign { comments, foreign_ }) = do
  (foreign_' :: Foreign QualifiedName) <- processForeign foreign_
  pure $ DeclForeign { comments, foreign_: foreign_' }

processDataHead :: DataHead SmartQualifiedName -> App (DataHead QualifiedName)
processDataHead (DataHead { dataHdName, dataHdVars }) = do
  (dataHdVars' :: Array (TypeVarBinding QualifiedName)) <- traverse processTypeVarBinding dataHdVars
  pure $ DataHead { dataHdName, dataHdVars: dataHdVars' }

processKind :: Kind SmartQualifiedName -> App (Kind QualifiedName)
processKind (KindName prop) = KindName <$> processSmartQualifiedNameKind prop
processKind (KindArr kindA kindB) = KindArr <$> (processKind kindA) <*> (processKind kindB)
processKind (KindRow kindA) = KindRow <$> (processKind kindA)

processTypeVarBinding :: TypeVarBinding SmartQualifiedName -> App (TypeVarBinding QualifiedName)
processTypeVarBinding (TypeVarKinded ident kind_) = TypeVarKinded ident <$> (processKind kind_)
processTypeVarBinding (TypeVarName ident) = pure $ TypeVarName ident

processDataCtor :: DataCtor SmartQualifiedName -> App (DataCtor QualifiedName)
processDataCtor (DataCtor { dataCtorName, dataCtorFields }) = do
  dataCtorFields' <- traverse processType dataCtorFields
  pure $ DataCtor { dataCtorName, dataCtorFields: dataCtorFields' }

processType :: Type SmartQualifiedName -> App (Type QualifiedName)
processType (TypeVar ident) = pure $ TypeVar ident
processType (TypeConstructor x) = TypeConstructor <$> processSmartQualifiedNameType x
processType (TypeWildcard) = pure TypeWildcard
processType (TypeHole ident) = pure $ TypeHole ident
processType (TypeString string) = pure $ TypeString string
processType (TypeRow row) = TypeRow <$> processRow row
processType (TypeRecord row) = TypeRecord <$> processRow row
processType (TypeApp typeA typeB) = TypeApp <$> processType typeA <*> processType typeB
processType (TypeForall bindings type_) = TypeForall <$> traverse processTypeVarBinding bindings <*> processType type_
processType (TypeArr typeA typeB) = TypeArr <$> processType typeA <*> processType typeB
processType (TypeKinded type_ kind_) = TypeKinded <$> processType type_ <*> processKind kind_
processType (TypeOp typeA opName typeB) = TypeOp <$> processType typeA <*> processSmartQualifiedNameTypeOp opName <*> processType typeB
processType (TypeConstrained constant type_) = TypeConstrained <$> processConstraint constant <*> processType type_

processInstanceHead :: InstanceHead SmartQualifiedName -> App (InstanceHead QualifiedName)
processInstanceHead instanceHead = do
  instConstraints' <- traverse processConstraint instanceHead.instConstraints
  instClass' <- processSmartQualifiedNameClass instanceHead.instClass
  instTypes' <- traverse processType instanceHead.instTypes
  pure $
    { instName: instanceHead.instName
    , instConstraints: instConstraints'
    , instClass: instClass'
    , instTypes: instTypes'
    }

processForeign :: Foreign SmartQualifiedName -> App (Foreign QualifiedName)
processForeign (ForeignValue { ident, type_ }) = do
  (type_' :: Type QualifiedName) <- processType type_
  pure $ ForeignValue { ident, type_: type_' }
processForeign (ForeignData { name, kind_ }) = do
  (kind_' :: Kind QualifiedName) <- processKind kind_
  pure $ ForeignData { name, kind_: kind_' }
processForeign (ForeignKind x) = pure $ ForeignKind x

processFixityFields :: FixityFields SmartQualifiedName -> App (FixityFields QualifiedName)
processFixityFields fixityFields = do
  operator <- processFixityOp fixityFields.operator
  pure $
    { keyword: fixityFields.keyword
    , precedence: fixityFields.precedence
    , operator
    }

processFixityOp :: FixityOp SmartQualifiedName -> App (FixityOp QualifiedName)
processFixityOp (FixityValue (Left qualifiedIdent) opName) = do
  qualifiedIdent' <- processSmartQualifiedNameValue qualifiedIdent
  pure $ FixityValue (Left qualifiedIdent') opName
processFixityOp (FixityValue (Right qualifiedProperName) opName) = do
  qualifiedProperName' <- processSmartQualifiedNameTypeConstructor qualifiedProperName
  pure $ FixityValue (Right qualifiedProperName') opName
processFixityOp (FixityType properName opName) = do
  properName' <- processSmartQualifiedNameType properName
  pure $ FixityType properName' opName

processRow :: Row SmartQualifiedName -> App (Row QualifiedName)
processRow (Row row) = do
  (rowLabels :: Array { label :: Label, type_ :: Type QualifiedName }) <- traverse (\rowItem -> { label: rowItem.label, type_: _ } <$> processType rowItem.type_) row.rowLabels
  rowTail <- traverse processType row.rowTail
  pure $ Row { rowLabels, rowTail }

processConstraint :: Constraint SmartQualifiedName -> App (Constraint QualifiedName)
processConstraint (Constraint constraint) = do
  className <- processSmartQualifiedNameClass constraint.className
  args <- traverse processType constraint.args
  pure $ Constraint { className, args }

processClassHead :: ClassHead SmartQualifiedName -> App (ClassHead QualifiedName)
processClassHead classHead = do
  vars <- traverse processTypeVarBinding classHead.vars
  super <- traverse processConstraint classHead.super
  pure $
    { name: classHead.name
    , vars
    , super
    , fundeps: classHead.fundeps
    }

processValueBindingFields :: ValueBindingFields SmartQualifiedName -> App (ValueBindingFields QualifiedName)
processValueBindingFields valueBindingFields = do
  binders <- traverse processBinder valueBindingFields.binders
  guarded <- processGuarded valueBindingFields.guarded
  pure $
    { name: valueBindingFields.name
    , binders
    , guarded
    }

processBinder :: Binder SmartQualifiedName -> App (Binder QualifiedName)
processBinder (BinderNamed binderNamed) = do
  binder <- processBinder binderNamed.binder
  pure $ BinderNamed { ident: binderNamed.ident, binder }
processBinder (BinderConstructor binderConstructor) = do
  name <- processSmartQualifiedNameTypeConstructor binderConstructor.name
  args <- traverse processBinder binderConstructor.args
  pure $ BinderConstructor { name, args }
processBinder (BinderArray binderArray) = do
  binderArray' <- traverse processBinder binderArray
  pure $ BinderArray binderArray'
processBinder (BinderRecord binderRecord) = do
  (binderRecord' :: Array (RecordLabeled (Binder QualifiedName))) <- traverse (traverse processBinder) binderRecord
  pure $ BinderRecord binderRecord'
processBinder (BinderTyped binder type_) = do
  binder' <- processBinder binder
  type_' <- processType type_
  pure $ BinderTyped binder' type_'
processBinder (BinderOp binderA qualifiedName binderB) = do
  binderA' <- processBinder binderA
  qualifiedName' <- processSmartQualifiedNameOp qualifiedName
  binderB' <- processBinder binderB
  pure $ BinderOp binderA' qualifiedName' binderB'
processBinder BinderWildcard = pure $ BinderWildcard
processBinder (BinderVar x) = pure $ BinderVar x
processBinder (BinderBoolean x) = pure $ BinderBoolean x
processBinder (BinderChar x) = pure $ BinderChar x
processBinder (BinderString x) = pure $ BinderString x
processBinder (BinderNumber x) = pure $ BinderNumber x

processGuarded :: Guarded SmartQualifiedName -> App (Guarded QualifiedName)
processGuarded (Unconditional where_) = Unconditional <$> processWhere where_
processGuarded (Guarded guardedExpr) = Guarded <$> traverse processGuardedExpr guardedExpr

processWhere :: Where SmartQualifiedName -> App (Where QualifiedName)
processWhere where_ = do
  expr <- processExpr where_.expr
  whereBindings <- traverse processLetBinding where_.whereBindings
  pure $
    { expr
    , whereBindings
    }

processLetBinding :: LetBinding SmartQualifiedName -> App (LetBinding QualifiedName)
processLetBinding (LetBindingSignature letBindingSignature) = do
  type_ <- processType letBindingSignature.type_
  pure $ LetBindingSignature { ident: letBindingSignature.ident, type_ }
processLetBinding (LetBindingName letBindingName) = LetBindingName <$> processValueBindingFields letBindingName
processLetBinding (LetBindingPattern letBindingPattern) = do
  binder <- processBinder letBindingPattern.binder
  where_ <- processWhere letBindingPattern.where_
  pure $ LetBindingPattern { binder, where_ }

processGuardedExpr :: GuardedExpr SmartQualifiedName -> App (GuardedExpr QualifiedName)
processGuardedExpr guardedExpr = do
  patterns <- traverse processPatternGuard guardedExpr.patterns
  where_ <- processWhere guardedExpr.where_
  pure $
    { patterns
    , where_
    }

processPatternGuard :: PatternGuard SmartQualifiedName -> App (PatternGuard QualifiedName)
processPatternGuard x = do
  binder <- traverse processBinder x.binder
  expr <- processExpr x.expr
  pure $
    { binder
    , expr
    }

processExpr :: Expr SmartQualifiedName -> App (Expr QualifiedName)
processExpr (ExprHole ident)                = pure $ ExprHole ident
processExpr ExprSection                     = pure $ ExprSection
processExpr (ExprIdent ident)               = ExprIdent <$> processSmartQualifiedNameValue ident
processExpr (ExprConstructor name)          = ExprConstructor <$> processSmartQualifiedNameTypeConstructor name
processExpr (ExprBoolean x)                 = pure $ ExprBoolean x
processExpr (ExprChar x)                    = pure $ ExprChar x
processExpr (ExprString x)                  = pure $ ExprString x
processExpr (ExprNumber x)                  = pure $ ExprNumber x
processExpr (ExprArray exprs)               = ExprArray <$> traverse processExpr exprs
processExpr (ExprRecord labels)             = ExprRecord <$> traverse (traverse processExpr) labels
processExpr (ExprTyped expr type_)          = ExprTyped <$> processExpr expr <*> processType type_
processExpr (ExprInfix exprA exprB exprC)   = ExprInfix <$> processExpr exprA <*> processExpr exprB <*> processExpr exprC
processExpr (ExprOp exprA opName exprB)     = ExprOp <$> processExpr exprA <*> processSmartQualifiedNameOp opName <*> processExpr exprA
processExpr (ExprOpName opName)             = ExprOpName <$> processSmartQualifiedNameOp opName
processExpr (ExprNegate expr)               = ExprNegate <$> processExpr expr
processExpr (ExprRecordAccessor rec)        = ExprRecordAccessor <$> processRecordAccessor rec
processExpr (ExprRecordUpdate expr updates) = ExprRecordUpdate <$> processExpr expr <*> traverse processRecordUpdate updates
processExpr (ExprApp exprA exprB)           = ExprApp <$> processExpr exprA <*> processExpr exprB
processExpr (ExprLambda x)                  = ExprLambda <$> processLambda x
processExpr (ExprIf x)                      = ExprIf <$> processIfThenElse x
processExpr (ExprCase x)                    = ExprCase <$> processCaseOf x
processExpr (ExprLet x)                     = ExprLet <$> processLetIn x
processExpr (ExprDo xs)                      = ExprDo <$> traverse processDoStatement xs
processExpr (ExprAdo x)                     = ExprAdo <$> processAdoBlock x

processRecordAccessor :: RecordAccessor SmartQualifiedName -> App (RecordAccessor QualifiedName)
processRecordAccessor x = do
  recExpr <- processExpr x.recExpr
  pure $
    { recExpr
    , recPath: x.recPath
    }

processRecordUpdate :: RecordUpdate SmartQualifiedName -> App (RecordUpdate QualifiedName)
processRecordUpdate (RecordUpdateLeaf label expr) = RecordUpdateLeaf label <$> processExpr expr
processRecordUpdate (RecordUpdateBranch label reqUp) = RecordUpdateBranch label <$> traverse processRecordUpdate reqUp

processLambda :: Lambda SmartQualifiedName -> App (Lambda QualifiedName)
processLambda x = do
  binders <- traverse processBinder x.binders
  body <- processExpr x.body
  pure $
    { binders
    , body
    }

processIfThenElse :: IfThenElse SmartQualifiedName -> App (IfThenElse QualifiedName)
processIfThenElse x = do
  cond <- processExpr x.cond
  true_ <- processExpr x.true_
  false_ <- processExpr x.false_
  pure $
    { cond
    , true_
    , false_
    }

processCaseOf :: CaseOf SmartQualifiedName -> App (CaseOf QualifiedName)
processCaseOf x = do
  head <- traverse processExpr x.head
  branches <- traverse (\branch -> { binders: _, body: _ } <$> traverse processBinder branch.binders <*> processGuarded branch.body) x.branches
  pure $
    { head
    , branches
    }

processLetIn :: LetIn SmartQualifiedName -> App (LetIn QualifiedName)
processLetIn x = do
  body <- processExpr x.body
  bindings <- traverse processLetBinding x.bindings
  pure $
    { body
    , bindings
    }

processDoStatement :: DoStatement SmartQualifiedName -> App (DoStatement QualifiedName)
processDoStatement (DoLet bindings) = DoLet <$> traverse processLetBinding bindings
processDoStatement (DoDiscard expr) = DoDiscard <$> processExpr expr
processDoStatement (DoBind bind) = DoBind <$> ({ binder: _, expr: _ } <$> processBinder bind.binder <*> processExpr bind.expr)

processAdoBlock :: AdoBlock SmartQualifiedName -> App (AdoBlock QualifiedName)
processAdoBlock x = do
  statements <- traverse processDoStatement x.statements
  result <- processExpr x.result
  pure $
    { statements
    , result
    }

processInstanceBinding :: InstanceBinding SmartQualifiedName -> App (InstanceBinding QualifiedName)
processInstanceBinding (InstanceBindingSignature x) = do
  type_ <- processType x.type_
  pure $ InstanceBindingSignature
    { ident: x.ident
    , type_
    }
processInstanceBinding (InstanceBindingName x) = InstanceBindingName <$> processValueBindingFields x

processInstance :: Instance SmartQualifiedName -> App (Instance QualifiedName)
processInstance x = do
  head <- processInstanceHead x.head
  body <- traverse processInstanceBinding x.body
  pure $
    { head
    , body
    }
