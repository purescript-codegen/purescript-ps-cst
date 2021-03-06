module Language.PS.CST.Printers where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Foldable (any, fold, foldMap, foldl, null)
import Data.Maybe (Maybe, maybe)
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Dodo (Doc, alignCurrentColumn, break, flexGroup, foldWithSeparator, indent, lines, paragraph, softBreak, space, spaceBreak, text, words, (<%>), (<+>))
import Dodo.Common (leadingComma, pursCurlies, pursParens, pursSquares)
import Language.PS.CST.Printers.PrintImports (printImports)
import Language.PS.CST.Printers.PrintModuleModuleNameAndExports (printModuleModuleNameAndExports)
import Language.PS.CST.Printers.TypeLevel (printConstraint, printConstraintList, printFixity, printFundep, printQualifiedName_AnyOpNameType, printQualifiedName_AnyProperNameType, printQualifiedName_Ident, printType, printType', printTypeVarBinding)
import Language.PS.CST.Printers.Utils (dot, dquotesIf, exprShouldBeOnNextLine, labelNeedsQuotes, maybeWrapInParentheses, parens, printAndConditionallyAddNewlinesBetween, printLabelledGroup, shouldBeNoNewlineBetweenDeclarations, shouldBeNoNewlineBetweenInstanceBindings, shouldBeNoNewlineBetweenLetBindings, unwrapText, (<%%>))
import Language.PS.CST.ReservedNames (appendUnderscoreIfReserved, quoteIfReserved)
import Language.PS.CST.Sugar.QualifiedName (nonQualifiedName)
import Language.PS.CST.Types.Declaration (Binder(..), PSConstraint(..), DataCtor(..), DataHead(..), Declaration(..), Expr(..), FixityOp(..), Foreign(..), Guarded(..), Instance, InstanceBinding(..), InstanceHead, LetBinding(..), RecordUpdate(..), PSType(..), ValueBindingFields)
import Language.PS.CST.Types.Leafs (Comments(..), DeclDeriveType(..), ProperName(..), RecordLabeled(..))
import Language.PS.CST.Types.Module (Module(..))

-- | This is an entry point
printModule :: Module -> Doc Void
printModule (Module { moduleName, imports, exports, declarations }) =
  foldWithSeparator (break <> break)
    [ printModuleModuleNameAndExports moduleName exports
    , printImports imports
    , printDeclarations declarations
    ] <> break

printDeclarations :: Array Declaration -> Doc Void
printDeclarations declarations = printAndConditionallyAddNewlinesBetween shouldBeNoNewlineBetweenDeclarations printDeclaration declarations

printComments :: Comments -> Doc Void
printComments (OneLineComments strings) = strings <#> (\x -> text "-- |" <+> text x) # lines
printComments (BlockComments strings) = text "{-" <> break <> indent (lines $ map text strings) <> break <> text "-}"

printMaybeComments :: Maybe Comments -> Doc Void -> Doc Void
printMaybeComments comments doc =
  lines
    [ maybe mempty printComments comments
    , doc
    ]

printDeclaration :: Declaration -> Doc Void
printDeclaration (DeclData { comments, head, constructors }) =
  printMaybeComments comments
  $ printAssignmentDecl "data" head (dataCtorToType <$> constructors)
printDeclaration (DeclType { comments, head, type_ }) =
  printMaybeComments comments
  $ printAssignmentDecl "type" head [ type_ ]

printDeclaration (DeclNewtype { comments, head, name, type_ }) =
  printMaybeComments comments
  $ printAssignmentDecl "newtype" head [ dataCtorToType dataCtor ]
  where
    dataCtor = DataCtor { dataCtorName: name
                        , dataCtorFields: [ type_ ]
                        }

printDeclaration (DeclFixity { comments, fixityFields: { keyword, precedence, operator } }) =
  let
    printFixityOp :: FixityOp -> Doc Void
    printFixityOp (FixityValue (Left qualifiedIdent) opName) = printQualifiedName_Ident qualifiedIdent <+> text "as" <+> (text <<< appendUnderscoreIfReserved <<< unwrap) opName
    printFixityOp (FixityValue (Right qualifiedPropName) opName) = printQualifiedName_AnyProperNameType qualifiedPropName <+> text "as" <+> (text <<< appendUnderscoreIfReserved <<< unwrap) opName
    printFixityOp (FixityType qualifiedPropName opName) = text "type" <+> printQualifiedName_AnyProperNameType qualifiedPropName <+> text "as" <+> (text <<< appendUnderscoreIfReserved <<< unwrap) opName
  in printMaybeComments comments (printFixity keyword <+> text (show precedence) <+> printFixityOp operator)
printDeclaration (DeclForeign { comments, foreign_ }) =
  printMaybeComments comments
  case foreign_ of
    ForeignValue { ident, type_ } ->
      printLabelledGroup
      (text "foreign import" <+> unwrapText ident)
      (flexGroup $ printType type_)
    ForeignData { name, kind_ } ->
      printLabelledGroup
      (text "foreign import data" <+> unwrapText name)
      (flexGroup $ printType' false kind_)
    ForeignKind { name } ->
      text "data" <+> unwrapText name

printDeclaration (DeclDerive { comments, deriveType, head: { instName, instConstraints, instClass, instTypes } }) =
  printMaybeComments comments $
  flexGroup $
  label
  <%%> indent (text "::" <+> flexGroup constraints')
  <%%> indent (arrow <+> flexGroup (alignCurrentColumn (printConstraint instConstraint)))

  where
    label = words
      [ text "derive"
      , deriveType'
      , text "instance"
      , unwrapText instName
      ]

    instConstraint = PSConstraint { className: instClass
                                , args: NonEmptyArray.toArray instTypes
                                }

    deriveType' =
      case deriveType of
        DeclDeriveType_Newtype -> text "newtype"
        DeclDeriveType_Ordinary -> mempty

    constraints' =
      fold
      $ printConstraintList
      <$> NonEmptyArray.fromArray instConstraints

    arrow = guard (not $ null instConstraints) (text "=>")

printDeclaration (DeclClass { comments, head: { name, vars, super, fundeps }, methods }) =
  let
    printedHeader = flexGroup $ foldWithSeparator space
      [ text "class"
      , case super of
             [] -> mempty
             [constraint] -> printConstraint constraint <+> text "<="
             constraints -> (alignCurrentColumn $ pursParens $ foldWithSeparator leadingComma $ map (flexGroup <<< printConstraint) constraints) <+> text "<="
      , unwrapText name
      , case vars of
             [] -> mempty
             _ -> alignCurrentColumn $ flexGroup $ foldWithSeparator spaceBreak $ map printTypeVarBinding vars
      , case fundeps of
             [] -> mempty
             _ -> text "|" <+> (alignCurrentColumn $ flexGroup $ foldWithSeparator (text ", ") $ map printFundep $ fundeps)
      ]
   in
    if null methods
      then printMaybeComments comments printedHeader
      else
        printMaybeComments comments
          ( printedHeader <+> (text "where") <> break <>
              ( indent
              $ paragraph
              $ map (\({ ident, type_ }) -> printLabelledGroup (unwrapText ident) (printType type_)) $ methods
              )
          )
printDeclaration (DeclInstanceChain { comments, instances }) = printMaybeComments comments (foldWithSeparator (break <> break <> text "else" <> break <> break) (map printInstance instances))
printDeclaration (DeclSignature { comments, ident, type_ }) =
  printMaybeComments comments (printLabelledGroup (unwrapText ident) (printType type_))
printDeclaration (DeclValue { comments, valueBindingFields }) =
  printMaybeComments comments (printValueBindingFields valueBindingFields)

printInstance :: Instance -> Doc Void
printInstance instance_ =
  printLabelledGroup
  (text "instance" <+> unwrapText instance_.head.instName)
  (flexGroup $ printType (instanceHeadToType instance_.head) <+> where_)
  <%> printedBody
  where
    where_ = if null instance_.body then mempty else text "where"

    printedBody = printAndConditionallyAddNewlinesBetween shouldBeNoNewlineBetweenInstanceBindings (indent <<< printInstanceBinding) instance_.body

printInstanceBinding :: InstanceBinding -> Doc Void
printInstanceBinding (InstanceBindingSignature { ident, type_ }) =
  printLabelledGroup (unwrapText ident) (printType type_)
printInstanceBinding (InstanceBindingName valueBindingFields) =
  printValueBindingFields valueBindingFields

printValueBindingFields :: ValueBindingFields -> Doc Void
printValueBindingFields = \{ name, binders, guarded } ->
  let
    printedBinders =
      if null binders
        then mempty
        else (paragraph $ map printBinderWithMaybeParens binders) <> space

    printedHead = (text <<< appendUnderscoreIfReserved <<< unwrap) name <+> printedBinders <> text "="
   in printGuarded printedHead guarded
  where
        printBinderWithMaybeParens x@(BinderConstructor { args: [] }) = printBinder x
        printBinderWithMaybeParens x@(BinderConstructor { args }) = parens $ printBinder x
        printBinderWithMaybeParens x = printBinder x

printGuarded :: Doc Void -> Guarded -> Doc Void
printGuarded printedHead guarded =
  case guarded of
    (Unconditional where_) ->
      case where_ of
        { expr, whereBindings: [] } ->
          if exprShouldBeOnNextLine expr
            then printedHead <> spaceBreak <>(indent $ printExpr expr)
            else printedHead <+> printExpr expr
        { expr, whereBindings } ->
          let
            printedBindings = indent (text "where" <> spaceBreak <>(printAndConditionallyAddNewlinesBetween shouldBeNoNewlineBetweenLetBindings printLetBinding whereBindings))
          in
            if exprShouldBeOnNextLine expr
              then printedHead <> spaceBreak <> (indent $ printExpr expr) <> spaceBreak <>printedBindings
              else printedHead <+> printExpr expr <> spaceBreak <>printedBindings
    (Guarded _) -> mempty -- TODO

printBinder :: Binder -> Doc Void
printBinder BinderWildcard = text "_"
printBinder (BinderVar ident) = (text <<< appendUnderscoreIfReserved <<< unwrap) ident
printBinder (BinderNamed { ident, binder }) = (text <<< appendUnderscoreIfReserved <<< unwrap) ident <> text "@" <> (parens $ printBinder binder)
printBinder (BinderConstructor { name, args: [] }) = printQualifiedName_AnyProperNameType name
printBinder (BinderConstructor { name, args }) = printQualifiedName_AnyProperNameType name <+> (paragraph $ map printBinder args)
printBinder (BinderBoolean boolean) = text $ show boolean
printBinder (BinderChar char) = text $ show char
printBinder (BinderString string) = text $ show string
printBinder (BinderNumber (Left int)) = text $ show int
printBinder (BinderNumber (Right number)) = text $ show number
printBinder (BinderArray binders) = text "[" <> (foldWithSeparator (text ", ") $ map printBinder binders) <> text "]"
printBinder (BinderRecord arrayRecordLabeledBinder) = foldWithSeparator (text ", ") $ map (printRecordLabeled printBinder) arrayRecordLabeledBinder
printBinder (BinderTyped binder type_) = printBinder binder <+> text "::" <+> printType type_
printBinder (BinderOp binderLeft operator binderRight) = printBinder binderLeft <+> printQualifiedName_AnyOpNameType operator <+> printBinder binderRight

printRecordLabeled :: ∀ a . (a -> Doc Void) -> RecordLabeled a -> Doc Void
printRecordLabeled _ (RecordPun ident) = (text <<< quoteIfReserved <<< unwrap) ident
printRecordLabeled print (RecordField label a) =
  (dquotesIf (labelNeedsQuotes label) <<< text <<< unwrap) label <> text ":" <+> print a

printExpr :: Expr -> Doc Void
printExpr =
  let
    processTopLevel printExprImplementation' expr =
        case expr of
             (ExprApp _ _) -> flexGroup $ printExprImplementation' expr
             (ExprArray _) -> flexGroup $ printExprImplementation' expr
             (ExprRecord _) -> flexGroup $ printExprImplementation' expr
             _ -> printExprImplementation' expr

    printExprImplementation (ExprHole hole) = text "?" <> (text <<< appendUnderscoreIfReserved <<< unwrap) hole
    printExprImplementation ExprSection = text "_"
    printExprImplementation (ExprIdent qualifiedIdent) = printQualifiedName_Ident qualifiedIdent
    printExprImplementation (ExprConstructor qualifiedPropName) = printQualifiedName_AnyProperNameType qualifiedPropName
    printExprImplementation (ExprBoolean boolean) = text $ show boolean
    printExprImplementation (ExprChar char) = text $ show char
    printExprImplementation (ExprString string) = text $ show string
    printExprImplementation (ExprNumber (Left int)) = text $ show int
    printExprImplementation (ExprNumber (Right num)) = text $ show num
    printExprImplementation (ExprArray array) = alignCurrentColumn $ pursSquares $ foldWithSeparator leadingComma (map (processTopLevel printExprImplementation) array)
    printExprImplementation (ExprRecord arrayRecordLabeled) = alignCurrentColumn $ pursCurlies $ foldWithSeparator leadingComma $ map (printRecordLabeled printExprImplementation) arrayRecordLabeled
    printExprImplementation (ExprTyped expr type_) = printExprImplementation expr <+> text "::" <+> printType type_
    printExprImplementation (ExprInfix exprLeft operator exprRight) = printExprImplementation exprLeft <+> printExprImplementation operator <+> printExprImplementation exprRight
    printExprImplementation (ExprOp exprLeft operator exprRight) = printExprImplementation exprLeft <+> printQualifiedName_AnyOpNameType operator <+> printExprImplementation exprRight
    printExprImplementation (ExprOpName opName) = printQualifiedName_AnyOpNameType opName
    printExprImplementation (ExprNegate expr) = text "-" <> printExprImplementation expr
    printExprImplementation (ExprRecordAccessor { recExpr, recPath }) = printExprImplementation recExpr <> text "." <> (foldWithSeparator dot $ map (\l -> dquotesIf (labelNeedsQuotes l) $ text $ appendUnderscoreIfReserved $ unwrap l) recPath)
    printExprImplementation (ExprRecordUpdate expr recordUpdates) = parens $ printExprImplementation expr <+> printRecordUpdates recordUpdates
    printExprImplementation (ExprApp exprLeft exprRight) =
      let
        doWrapRight =
          case exprRight of
            (ExprApp _ _) -> true -- always wrap right side application
            (ExprInfix _ _ _) -> true
            (ExprOp _ _ _) -> true
            _ -> false
      in alignCurrentColumn $ foldWithSeparator spaceBreak $ [ printExprImplementation exprLeft, maybeWrapInParentheses doWrapRight (printExprImplementation exprRight) ]
    printExprImplementation (ExprLambda { binders, body }) = (parens $ paragraph $ map printBinder binders) <+> text "=" <+> printExprImplementation body
    printExprImplementation (ExprIf { cond, true_, false_ }) = foldWithSeparator spaceBreak
      [ if exprShouldBeOnNextLine cond
          then text "if" <> break <> (indent $ printExprImplementation cond)
          else text "if" <+> printExprImplementation cond
      , if exprShouldBeOnNextLine true_
          then indent $ text "then" <> break <> (indent $ printExprImplementation true_)
          else indent $ text "then" <+> printExprImplementation true_
      , if exprShouldBeOnNextLine false_
          then indent $ text "else" <> break <> (indent $ printExprImplementation false_)
          else indent $ text "else" <+> printExprImplementation false_
      ]
    printExprImplementation (ExprCase { head, branches }) =
      let
        headShouldBeMultiline =
          head `flip any` (
            case _ of
              ExprIf _ -> true
              ExprCase _ -> true
              ExprLet _ -> true
              ExprDo _ -> true
              ExprAdo _ -> true
              _ -> false
          )

        headDocs :: Array (Doc Void)
        headDocs = NonEmptyArray.toArray $ map printExprImplementation head
      in
        if headShouldBeMultiline
          then foldWithSeparator spaceBreak
            [ text "case"
            , foldWithSeparator softBreak $
              Array.zipWith
              (<>)
              ([text "  "] <> Array.replicate (Array.length headDocs - 1) (text ", "))
              (map alignCurrentColumn headDocs)
            , text "of"
            , indent $ lines $ map printBranch branches
            ]
          else
            text "case" <+> foldWithSeparator (text ", ") headDocs <+> text "of"
            <> break <>
            (indent $ lines $ map printBranch branches)
    printExprImplementation (ExprLet { bindings, body }) = alignCurrentColumn $ foldWithSeparator break
      [ text "let"
      , indent (printAndConditionallyAddNewlinesBetween shouldBeNoNewlineBetweenLetBindings printLetBinding bindings)
      , text " in"
      , indent (processTopLevel printExprImplementation body)
      ]
    printExprImplementation (ExprDo doStatements) = mempty -- TODO
    printExprImplementation (ExprAdo { statements, result }) = mempty -- TODO
  in processTopLevel printExprImplementation

printBranch :: { binders :: NonEmptyArray Binder, body :: Guarded } -> Doc Void
printBranch { binders, body } =
  let
    printedHead = (foldWithSeparator (text ", ") $ map printBinder binders) <+> text "->"
   in printGuarded printedHead body

printLetBinding :: LetBinding -> Doc Void
printLetBinding (LetBindingSignature { ident, type_ }) =
  printLabelledGroup (unwrapText ident) (printType type_)
printLetBinding (LetBindingName valueBindingFields) =
  printValueBindingFields valueBindingFields
printLetBinding (LetBindingPattern { binder, where_: { expr, whereBindings } }) = printBinder binder <> break <> printExpr expr <> spaceBreak <>text "where" <> spaceBreak <>(paragraph $ map printLetBinding whereBindings)

printRecordUpdates :: NonEmptyArray RecordUpdate -> Doc Void
printRecordUpdates recordUpdates = text "{" <+> (foldWithSeparator (text ",") $ map printRecordUpdate recordUpdates) <+> text "}"

printRecordUpdate :: RecordUpdate -> Doc Void
printRecordUpdate (RecordUpdateLeaf label expr) = (text <<< appendUnderscoreIfReserved <<< unwrap) label <+> text "=" <+> printExpr expr
printRecordUpdate (RecordUpdateBranch label recordUpdates) = (text <<< appendUnderscoreIfReserved <<< unwrap) label <+> text "=" <+> printRecordUpdates recordUpdates

printAssignmentDecl
  :: String
  -> DataHead
  -> Array PSType
  -> Doc Void
printAssignmentDecl reservedWord (DataHead { dataHdName, dataHdVars }) types =
  (flexGroup $
   text reservedWord <+> (unwrapText dataHdName <+> words (printTypeVarBinding <$> dataHdVars))
   <> foldMap printCtors (NonEmptyArray.fromArray types)
  )

  where
    printCtors types' =
      spaceBreak <>
      indent (text "= " <> foldWithSeparator sep (flexGroup <<< printType' true <$> types'))

    sep = spaceBreak <> text "| "

dataCtorToType :: DataCtor -> PSType
dataCtorToType (DataCtor ctor) = foldl TypeApp initType ctor.dataCtorFields
  where
    initType =
      TypeConstructor (nonQualifiedName (coerceProperName ctor.dataCtorName))

-- TODO add printing of constraints
instanceHeadToType :: InstanceHead -> PSType
instanceHeadToType inst = foldl TypeApp initType inst.instTypes
  where
    initType = TypeConstructor (coerceProperName <$> inst.instClass)

coerceProperName :: forall p1 p2. ProperName p1 -> ProperName p2
coerceProperName (ProperName p) = ProperName p
