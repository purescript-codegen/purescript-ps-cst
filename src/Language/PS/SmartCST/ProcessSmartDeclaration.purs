module Language.PS.SmartCST.ProcessSmartDeclaration where

import Data.Tuple.Nested
import Language.PS.CST.Types.Leafs
import Language.PS.CST.Types.Module
import Language.PS.CST.Types.QualifiedName
import Language.PS.CST.Sugar.QualifiedName
import Language.PS.SmartCST.ProcessSmartDeclaration.Utils
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
import Language.PS.CST.Types.Declaration as CST.Declaration
import Language.PS.SmartCST.Types.Declaration as SmartCST.Declaration

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
  smartCreateNew =
  case _ of
       SmartQualifiedName__Ignore name -> pure $ QualifiedName { qualModule: Nothing, qualName: name }
       SmartQualifiedName__Full originalModule name -> do
          modify_ $ findAndModifyOrNew
            (\(ImportDecl import_) ->
              case import_.qualification of
                   Nothing -> false
                   Just moduleName -> originalModule == moduleName
            )
            (\(ImportDecl import_) ->
              ImportDecl $ import_
                { names =
                  findAndModifyOrNew
                  (smartFindEqToName name)
                  (smartModify name)
                  (\_ -> smartCreateNew name)
                  import_.names
                }
            )
            (\_ -> ImportDecl
              { moduleName: originalModule
              , names: [smartCreateNew name]
              , qualification: Just originalModule
              }
            )
          pure $ QualifiedName { qualModule: Just originalModule, qualName: name }
       SmartQualifiedName__Simple originalModule name -> do
          modify_ $ findAndModifyOrNew
            (\(ImportDecl import_) ->
              case import_.qualification of
                   Nothing -> import_.moduleName == originalModule
                   Just _ -> false
            )
            (\(ImportDecl import_) ->
              ImportDecl $ import_
                { names =
                  findAndModifyOrNew
                  (smartFindEqToName name)
                  (smartModify name)
                  (\_ -> smartCreateNew name)
                  import_.names
                }
            )
            (\_ -> ImportDecl
              { moduleName: originalModule
              , names: [smartCreateNew name]
              , qualification: Nothing
              }
            )
          pure $ QualifiedName { qualModule: Nothing, qualName: name }
       SmartQualifiedName__Custom originalModule customModule name -> do
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
                  (smartFindEqToName name)
                  (smartModify name)
                  (\_ -> smartCreateNew name)
                  import_.names
                }
            )
            (\_ -> ImportDecl
              { moduleName: originalModule
              , names: [smartCreateNew name]
              , qualification: Just customModule
              }
            )
          pure $ QualifiedName { qualModule: Just customModule, qualName: name }

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
processSmartQualifiedNameTypeConstructor :: SmartQualifiedName ConstructorProperName -> App (QualifiedName (ProperName ProperNameType_ConstructorName))
processSmartQualifiedNameTypeConstructor = processSmartQualifiedName
  (\(ConstructorProperName constructorProperName) ->
    case _ of
      ImportType name' _ -> name' == constructorProperName.type_
      _ -> false
  )
  (\(ConstructorProperName constructorProperName) _currentImport -> ImportType constructorProperName.type_ (Just DataAll))
  (\(ConstructorProperName constructorProperName) -> ImportType constructorProperName.type_ (Just DataAll))
  <#> map (map (\(ConstructorProperName constructorProperName) -> constructorProperName.constructor))

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

processDeclarations :: Array SmartCST.Declaration.Declaration -> Array CST.Declaration.Declaration /\ Array ImportDecl
processDeclarations x = runState (traverse processDeclaration x) []

processDeclaration :: SmartCST.Declaration.Declaration -> App CST.Declaration.Declaration
processDeclaration (SmartCST.Declaration.DeclData { comments, head, constructors }) = do
  (head' :: CST.Declaration.DataHead) <- processDataHead head
  (constructors' :: Array (CST.Declaration.DataCtor)) <- traverse processDataCtor constructors
  pure $ CST.Declaration.DeclData { comments, head: head', constructors: constructors' }
processDeclaration (SmartCST.Declaration.DeclType { comments, head, type_ }) = do
  (head' :: CST.Declaration.DataHead) <- processDataHead head
  (type_' :: CST.Declaration.Type) <- processType type_
  pure $ CST.Declaration.DeclType { comments, head: head', type_: type_' }
processDeclaration (SmartCST.Declaration.DeclNewtype { comments, head, name, type_ }) = do
  (head' :: CST.Declaration.DataHead) <- processDataHead head
  (type_' :: CST.Declaration.Type) <- processType type_
  pure $ CST.Declaration.DeclNewtype { comments, head: head', name, type_: type_' }
processDeclaration (SmartCST.Declaration.DeclClass { comments, head, methods }) = do
  (head' :: CST.Declaration.ClassHead) <- processClassHead head
  (methods' :: Array { ident :: Ident, type_ :: CST.Declaration.Type }) <- traverse (\{ ident, type_ } -> { ident, type_: _ } <$> processType type_) methods
  pure $ CST.Declaration.DeclClass { comments, head: head', methods: methods' }
processDeclaration (SmartCST.Declaration.DeclInstanceChain { comments, instances }) = do
  (instances' :: NonEmptyArray (CST.Declaration.Instance)) <- traverse processInstance instances
  pure $ CST.Declaration.DeclInstanceChain { comments, instances: instances' }
processDeclaration (SmartCST.Declaration.DeclDerive { comments, deriveType, head }) = do
  (head' :: CST.Declaration.InstanceHead) <- processInstanceHead head
  pure $ CST.Declaration.DeclDerive { comments, deriveType, head: head' }
processDeclaration (SmartCST.Declaration.DeclSignature { comments, ident, type_ }) = do
  (type_' :: CST.Declaration.Type) <- processType type_
  pure $ CST.Declaration.DeclSignature { comments, ident, type_: type_' }
processDeclaration (SmartCST.Declaration.DeclValue { comments, valueBindingFields }) = do
  (valueBindingFields' :: CST.Declaration.ValueBindingFields) <- processValueBindingFields valueBindingFields
  pure $ CST.Declaration.DeclValue { comments, valueBindingFields: valueBindingFields' }
processDeclaration (SmartCST.Declaration.DeclFixity { comments, fixityFields }) = do
  (fixityFields' :: CST.Declaration.FixityFields) <- processFixityFields fixityFields
  pure $ CST.Declaration.DeclFixity { comments, fixityFields: fixityFields' }
processDeclaration (SmartCST.Declaration.DeclForeign { comments, foreign_ }) = do
  (foreign_' :: CST.Declaration.Foreign) <- processForeign foreign_
  pure $ CST.Declaration.DeclForeign { comments, foreign_: foreign_' }

processDataHead :: SmartCST.Declaration.DataHead -> App CST.Declaration.DataHead
processDataHead (SmartCST.Declaration.DataHead { dataHdName, dataHdVars }) = do
  (dataHdVars' :: Array (CST.Declaration.TypeVarBinding)) <- traverse processTypeVarBinding dataHdVars
  pure $ CST.Declaration.DataHead { dataHdName, dataHdVars: dataHdVars' }

processKind :: SmartCST.Declaration.Kind -> App CST.Declaration.Kind
processKind (SmartCST.Declaration.KindName prop) = CST.Declaration.KindName <$> processSmartQualifiedNameKind prop
processKind (SmartCST.Declaration.KindArr kindA kindB) = CST.Declaration.KindArr <$> (processKind kindA) <*> (processKind kindB)
processKind (SmartCST.Declaration.KindRow kindA) = CST.Declaration.KindRow <$> (processKind kindA)

processTypeVarBinding :: SmartCST.Declaration.TypeVarBinding -> App CST.Declaration.TypeVarBinding
processTypeVarBinding (SmartCST.Declaration.TypeVarKinded ident kind_) = CST.Declaration.TypeVarKinded ident <$> (processKind kind_)
processTypeVarBinding (SmartCST.Declaration.TypeVarName ident) = pure $ CST.Declaration.TypeVarName ident

processDataCtor :: SmartCST.Declaration.DataCtor -> App CST.Declaration.DataCtor
processDataCtor (SmartCST.Declaration.DataCtor { dataCtorName, dataCtorFields }) = do
  dataCtorFields' <- traverse processType dataCtorFields
  pure $ CST.Declaration.DataCtor { dataCtorName, dataCtorFields: dataCtorFields' }

processType :: SmartCST.Declaration.Type -> App CST.Declaration.Type
processType (SmartCST.Declaration.TypeVar ident) = pure $ CST.Declaration.TypeVar ident
processType (SmartCST.Declaration.TypeConstructor x) = CST.Declaration.TypeConstructor <$> processSmartQualifiedNameType x
processType (SmartCST.Declaration.TypeWildcard) = pure CST.Declaration.TypeWildcard
processType (SmartCST.Declaration.TypeHole ident) = pure $ CST.Declaration.TypeHole ident
processType (SmartCST.Declaration.TypeString string) = pure $ CST.Declaration.TypeString string
processType (SmartCST.Declaration.TypeRow row) = CST.Declaration.TypeRow <$> processRow row
processType (SmartCST.Declaration.TypeRecord row) = CST.Declaration.TypeRecord <$> processRow row
processType (SmartCST.Declaration.TypeApp typeA typeB) = CST.Declaration.TypeApp <$> processType typeA <*> processType typeB
processType (SmartCST.Declaration.TypeForall bindings type_) = CST.Declaration.TypeForall <$> traverse processTypeVarBinding bindings <*> processType type_
processType (SmartCST.Declaration.TypeArr typeA typeB) = CST.Declaration.TypeArr <$> processType typeA <*> processType typeB
processType (SmartCST.Declaration.TypeKinded type_ kind_) = CST.Declaration.TypeKinded <$> processType type_ <*> processKind kind_
processType (SmartCST.Declaration.TypeOp typeA opName typeB) = CST.Declaration.TypeOp <$> processType typeA <*> processSmartQualifiedNameTypeOp opName <*> processType typeB
processType (SmartCST.Declaration.TypeConstrained constant type_) = CST.Declaration.TypeConstrained <$> processConstraint constant <*> processType type_

processInstanceHead :: SmartCST.Declaration.InstanceHead -> App CST.Declaration.InstanceHead
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

processForeign :: SmartCST.Declaration.Foreign -> App CST.Declaration.Foreign
processForeign (SmartCST.Declaration.ForeignValue { ident, type_ }) = do
  (type_' :: CST.Declaration.Type) <- processType type_
  pure $ CST.Declaration.ForeignValue { ident, type_: type_' }
processForeign (SmartCST.Declaration.ForeignData { name, kind_ }) = do
  (kind_' :: CST.Declaration.Kind) <- processKind kind_
  pure $ CST.Declaration.ForeignData { name, kind_: kind_' }
processForeign (SmartCST.Declaration.ForeignKind x) = pure $ CST.Declaration.ForeignKind x

processFixityFields :: SmartCST.Declaration.FixityFields -> App CST.Declaration.FixityFields
processFixityFields fixityFields = do
  operator <- processFixityOp fixityFields.operator
  pure $
    { keyword: fixityFields.keyword
    , precedence: fixityFields.precedence
    , operator
    }

processFixityOp :: SmartCST.Declaration.FixityOp -> App CST.Declaration.FixityOp
processFixityOp (SmartCST.Declaration.FixityValue (Left qualifiedIdent) opName) = do
  qualifiedIdent' <- processSmartQualifiedNameValue qualifiedIdent
  pure $ CST.Declaration.FixityValue (Left qualifiedIdent') opName
processFixityOp (SmartCST.Declaration.FixityValue (Right qualifiedProperName) opName) = do
  qualifiedProperName' <- processSmartQualifiedNameTypeConstructor qualifiedProperName
  pure $ CST.Declaration.FixityValue (Right qualifiedProperName') opName
processFixityOp (SmartCST.Declaration.FixityType properName opName) = do
  properName' <- processSmartQualifiedNameType properName
  pure $ CST.Declaration.FixityType properName' opName

processRow :: SmartCST.Declaration.Row -> App CST.Declaration.Row
processRow row = do
  (rowLabels :: Array { label :: Label, type_ :: CST.Declaration.Type }) <- traverse (\rowItem -> { label: rowItem.label, type_: _ } <$> processType rowItem.type_) row.rowLabels
  rowTail <- traverse processType row.rowTail
  pure { rowLabels, rowTail }

processConstraint :: SmartCST.Declaration.Constraint -> App CST.Declaration.Constraint
processConstraint (SmartCST.Declaration.Constraint constraint) = do
  className <- processSmartQualifiedNameClass constraint.className
  args <- traverse processType constraint.args
  pure $ CST.Declaration.Constraint { className, args }

processClassHead :: SmartCST.Declaration.ClassHead -> App CST.Declaration.ClassHead
processClassHead classHead = do
  vars <- traverse processTypeVarBinding classHead.vars
  super <- traverse processConstraint classHead.super
  pure $
    { name: classHead.name
    , vars
    , super
    , fundeps: classHead.fundeps
    }

processValueBindingFields :: SmartCST.Declaration.ValueBindingFields -> App CST.Declaration.ValueBindingFields
processValueBindingFields valueBindingFields = do
  binders <- traverse processBinder valueBindingFields.binders
  guarded <- processGuarded valueBindingFields.guarded
  pure $
    { name: valueBindingFields.name
    , binders
    , guarded
    }

processBinder :: SmartCST.Declaration.Binder -> App CST.Declaration.Binder
processBinder (SmartCST.Declaration.BinderNamed binderNamed) = do
  binder <- processBinder binderNamed.binder
  pure $ CST.Declaration.BinderNamed { ident: binderNamed.ident, binder }
processBinder (SmartCST.Declaration.BinderConstructor binderConstructor) = do
  name <- processSmartQualifiedNameTypeConstructor binderConstructor.name
  args <- traverse processBinder binderConstructor.args
  pure $ CST.Declaration.BinderConstructor { name, args }
processBinder (SmartCST.Declaration.BinderArray binderArray) = do
  binderArray' <- traverse processBinder binderArray
  pure $ CST.Declaration.BinderArray binderArray'
processBinder (SmartCST.Declaration.BinderRecord binderRecord) = do
  (binderRecord' :: Array (RecordLabeled (CST.Declaration.Binder))) <- traverse (traverse processBinder) binderRecord
  pure $ CST.Declaration.BinderRecord binderRecord'
processBinder (SmartCST.Declaration.BinderTyped binder type_) = do
  binder' <- processBinder binder
  type_' <- processType type_
  pure $ CST.Declaration.BinderTyped binder' type_'
processBinder (SmartCST.Declaration.BinderOp binderA qualifiedName binderB) = do
  binderA' <- processBinder binderA
  qualifiedName' <- processSmartQualifiedNameOp qualifiedName
  binderB' <- processBinder binderB
  pure $ CST.Declaration.BinderOp binderA' qualifiedName' binderB'
processBinder SmartCST.Declaration.BinderWildcard = pure $ CST.Declaration.BinderWildcard
processBinder (SmartCST.Declaration.BinderVar x) = pure $ CST.Declaration.BinderVar x
processBinder (SmartCST.Declaration.BinderBoolean x) = pure $ CST.Declaration.BinderBoolean x
processBinder (SmartCST.Declaration.BinderChar x) = pure $ CST.Declaration.BinderChar x
processBinder (SmartCST.Declaration.BinderString x) = pure $ CST.Declaration.BinderString x
processBinder (SmartCST.Declaration.BinderNumber x) = pure $ CST.Declaration.BinderNumber x

processGuarded :: SmartCST.Declaration.Guarded -> App CST.Declaration.Guarded
processGuarded (SmartCST.Declaration.Unconditional where_) = CST.Declaration.Unconditional <$> processWhere where_
processGuarded (SmartCST.Declaration.Guarded guardedExpr) = CST.Declaration.Guarded <$> traverse processGuardedExpr guardedExpr

processWhere :: SmartCST.Declaration.Where -> App CST.Declaration.Where
processWhere where_ = do
  expr <- processExpr where_.expr
  whereBindings <- traverse processLetBinding where_.whereBindings
  pure $
    { expr
    , whereBindings
    }

processLetBinding :: SmartCST.Declaration.LetBinding -> App CST.Declaration.LetBinding
processLetBinding (SmartCST.Declaration.LetBindingSignature letBindingSignature) = do
  type_ <- processType letBindingSignature.type_
  pure $ CST.Declaration.LetBindingSignature { ident: letBindingSignature.ident, type_ }
processLetBinding (SmartCST.Declaration.LetBindingName letBindingName) = CST.Declaration.LetBindingName <$> processValueBindingFields letBindingName
processLetBinding (SmartCST.Declaration.LetBindingPattern letBindingPattern) = do
  binder <- processBinder letBindingPattern.binder
  where_ <- processWhere letBindingPattern.where_
  pure $ CST.Declaration.LetBindingPattern { binder, where_ }

processGuardedExpr :: SmartCST.Declaration.GuardedExpr -> App CST.Declaration.GuardedExpr
processGuardedExpr guardedExpr = do
  patterns <- traverse processPatternGuard guardedExpr.patterns
  where_ <- processWhere guardedExpr.where_
  pure $
    { patterns
    , where_
    }

processPatternGuard :: SmartCST.Declaration.PatternGuard -> App CST.Declaration.PatternGuard
processPatternGuard x = do
  binder <- traverse processBinder x.binder
  expr <- processExpr x.expr
  pure $
    { binder
    , expr
    }

processExpr :: SmartCST.Declaration.Expr -> App CST.Declaration.Expr
processExpr (SmartCST.Declaration.ExprHole ident)                = pure $ CST.Declaration.ExprHole ident
processExpr SmartCST.Declaration.ExprSection                     = pure $ CST.Declaration.ExprSection
processExpr (SmartCST.Declaration.ExprIdent ident)               = CST.Declaration.ExprIdent <$> processSmartQualifiedNameValue ident
processExpr (SmartCST.Declaration.ExprVar ident)                 = pure $ CST.Declaration.ExprIdent (nonQualifiedName ident)
processExpr (SmartCST.Declaration.ExprConstructor name)          = CST.Declaration.ExprConstructor <$> processSmartQualifiedNameTypeConstructor name
processExpr (SmartCST.Declaration.ExprBoolean x)                 = pure $ CST.Declaration.ExprBoolean x
processExpr (SmartCST.Declaration.ExprChar x)                    = pure $ CST.Declaration.ExprChar x
processExpr (SmartCST.Declaration.ExprString x)                  = pure $ CST.Declaration.ExprString x
processExpr (SmartCST.Declaration.ExprNumber x)                  = pure $ CST.Declaration.ExprNumber x
processExpr (SmartCST.Declaration.ExprArray exprs)               = CST.Declaration.ExprArray <$> traverse processExpr exprs
processExpr (SmartCST.Declaration.ExprRecord labels)             = CST.Declaration.ExprRecord <$> traverse (traverse processExpr) labels
processExpr (SmartCST.Declaration.ExprTyped expr type_)          = CST.Declaration.ExprTyped <$> processExpr expr <*> processType type_
processExpr (SmartCST.Declaration.ExprInfix exprA exprB exprC)   = CST.Declaration.ExprInfix <$> processExpr exprA <*> processExpr exprB <*> processExpr exprC
processExpr (SmartCST.Declaration.ExprOp exprA opName exprB)     = CST.Declaration.ExprOp <$> processExpr exprA <*> processSmartQualifiedNameOp opName <*> processExpr exprA
processExpr (SmartCST.Declaration.ExprOpName opName)             = CST.Declaration.ExprOpName <$> processSmartQualifiedNameOp opName
processExpr (SmartCST.Declaration.ExprNegate expr)               = CST.Declaration.ExprNegate <$> processExpr expr
processExpr (SmartCST.Declaration.ExprRecordAccessor rec)        = CST.Declaration.ExprRecordAccessor <$> processRecordAccessor rec
processExpr (SmartCST.Declaration.ExprRecordUpdate expr updates) = CST.Declaration.ExprRecordUpdate <$> processExpr expr <*> traverse processRecordUpdate updates
processExpr (SmartCST.Declaration.ExprApp exprA exprB)           = CST.Declaration.ExprApp <$> processExpr exprA <*> processExpr exprB
processExpr (SmartCST.Declaration.ExprLambda x)                  = CST.Declaration.ExprLambda <$> processLambda x
processExpr (SmartCST.Declaration.ExprIf x)                      = CST.Declaration.ExprIf <$> processIfThenElse x
processExpr (SmartCST.Declaration.ExprCase x)                    = CST.Declaration.ExprCase <$> processCaseOf x
processExpr (SmartCST.Declaration.ExprLet x)                     = CST.Declaration.ExprLet <$> processLetIn x
processExpr (SmartCST.Declaration.ExprDo xs)                     = CST.Declaration.ExprDo <$> traverse processDoStatement xs
processExpr (SmartCST.Declaration.ExprAdo x)                     = CST.Declaration.ExprAdo <$> processAdoBlock x

processRecordAccessor :: SmartCST.Declaration.RecordAccessor -> App CST.Declaration.RecordAccessor
processRecordAccessor x = do
  recExpr <- processExpr x.recExpr
  pure $
    { recExpr
    , recPath: x.recPath
    }

processRecordUpdate :: SmartCST.Declaration.RecordUpdate -> App CST.Declaration.RecordUpdate
processRecordUpdate (SmartCST.Declaration.RecordUpdateLeaf label expr) = CST.Declaration.RecordUpdateLeaf label <$> processExpr expr
processRecordUpdate (SmartCST.Declaration.RecordUpdateBranch label reqUp) = CST.Declaration.RecordUpdateBranch label <$> traverse processRecordUpdate reqUp

processLambda :: SmartCST.Declaration.Lambda -> App CST.Declaration.Lambda
processLambda x = do
  binders <- traverse processBinder x.binders
  body <- processExpr x.body
  pure $
    { binders
    , body
    }

processIfThenElse :: SmartCST.Declaration.IfThenElse -> App CST.Declaration.IfThenElse
processIfThenElse x = do
  cond <- processExpr x.cond
  true_ <- processExpr x.true_
  false_ <- processExpr x.false_
  pure $
    { cond
    , true_
    , false_
    }

processCaseOf :: SmartCST.Declaration.CaseOf -> App CST.Declaration.CaseOf
processCaseOf x = do
  head <- traverse processExpr x.head
  branches <- traverse (\branch -> { binders: _, body: _ } <$> traverse processBinder branch.binders <*> processGuarded branch.body) x.branches
  pure $
    { head
    , branches
    }

processLetIn :: SmartCST.Declaration.LetIn -> App CST.Declaration.LetIn
processLetIn x = do
  body <- processExpr x.body
  bindings <- traverse processLetBinding x.bindings
  pure $
    { body
    , bindings
    }

processDoStatement :: SmartCST.Declaration.DoStatement -> App CST.Declaration.DoStatement
processDoStatement (SmartCST.Declaration.DoLet bindings) = CST.Declaration.DoLet <$> traverse processLetBinding bindings
processDoStatement (SmartCST.Declaration.DoDiscard expr) = CST.Declaration.DoDiscard <$> processExpr expr
processDoStatement (SmartCST.Declaration.DoBind bind) = CST.Declaration.DoBind <$> ({ binder: _, expr: _ } <$> processBinder bind.binder <*> processExpr bind.expr)

processAdoBlock :: SmartCST.Declaration.AdoBlock -> App CST.Declaration.AdoBlock
processAdoBlock x = do
  statements <- traverse processDoStatement x.statements
  result <- processExpr x.result
  pure $
    { statements
    , result
    }

processInstanceBinding :: SmartCST.Declaration.InstanceBinding -> App CST.Declaration.InstanceBinding
processInstanceBinding (SmartCST.Declaration.InstanceBindingSignature x) = do
  type_ <- processType x.type_
  pure $ CST.Declaration.InstanceBindingSignature
    { ident: x.ident
    , type_
    }
processInstanceBinding (SmartCST.Declaration.InstanceBindingName x) = CST.Declaration.InstanceBindingName <$> processValueBindingFields x

processInstance :: SmartCST.Declaration.Instance -> App CST.Declaration.Instance
processInstance x = do
  head <- processInstanceHead x.head
  body <- traverse processInstanceBinding x.body
  pure $
    { head
    , body
    }
