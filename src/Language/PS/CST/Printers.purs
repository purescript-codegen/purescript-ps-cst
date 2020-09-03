module Language.PS.CST.Printers where

import Language.PS.CST.Printers.Utils (exprShouldBeOnNextLine, maybeWrapInParentheses, printAndConditionallyAddNewlinesBetween, shouldBeNoNewlineBetweenDeclarations, shouldBeNoNewlineBetweenInstanceBindings, shouldBeNoNewlineBetweenLetBindings)
import Prelude
import PrettyprinterRenderable (Doc, align, concatWith, concatWithNonEmpty, emptyDoc, flatAlt, group, hardline, indent, line, line', space, surround, surroundOmittingEmpty, text, vcat, vcatOmittingEmpty, vcatOmittingEmptyNonEmpty, vsep, (<+>))
import PrettyprinterRenderable.Symbols.String (dot, parens)

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Foldable (any, null)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap)
import Language.PS.CST.Printers.PrintImports (printImports)
import Language.PS.CST.Printers.PrintModuleModuleNameAndExports (printModuleModuleNameAndExports)
import Language.PS.CST.Printers.TypeLevel (printConstraint, printDataCtor, printDataHead, printFixity, printFundep, printKind, printQualifiedName_AnyOpNameType, printQualifiedName_AnyProperNameType, printQualifiedName_Ident, printType, printTypeVarBinding)
import Language.PS.CST.ReservedNames (appendUnderscoreIfReserved, quoteIfReserved)
import Language.PS.CST.Types.Declaration (Binder(..), Declaration(..), Expr(..), FixityOp(..), Foreign(..), Guarded(..), Instance, InstanceBinding(..), LetBinding(..), RecordUpdate(..), Type(..), ValueBindingFields)
import Language.PS.CST.Types.Leafs (Comments(..), DeclDeriveType(..), RecordLabeled(..))
import Language.PS.CST.Types.Module (Module(..))
import PrettyprinterRenderable as Pretty
import PrettyprinterRenderable.Code.Purescript (encloseSep)

printModuleToString :: Int -> Module -> String
printModuleToString width = Pretty.render width <<< printModule

printModule :: Module -> Doc String
printModule (Module { moduleName, imports, exports, declarations }) =
  concatWith (surroundOmittingEmpty (hardline <> hardline))
    [ printModuleModuleNameAndExports moduleName exports
    , printImports imports
    , printDeclarations declarations
    ] <> hardline

printDeclarations :: Array (Declaration) -> Doc String
printDeclarations declarations = printAndConditionallyAddNewlinesBetween shouldBeNoNewlineBetweenDeclarations printDeclaration declarations

printComments :: Comments -> Doc String
printComments (OneLineComments strings) = strings <#> (\x -> text "-- |" <+> text x) # vcatOmittingEmpty
printComments (BlockComments strings) = text "{-" <> line <> indent 2 (vcatOmittingEmpty $ map text strings) <> line <> text "-}"

printMaybeComments :: Maybe Comments -> Doc String -> Doc String
printMaybeComments comments doc =
  vcatOmittingEmpty
    [ maybe emptyDoc printComments comments
    , doc
    ]

printDeclaration :: Declaration -> Doc String
printDeclaration (DeclData { comments, head, constructors: [] }) = printMaybeComments comments (printDataHead (text "data") head)
printDeclaration (DeclData { comments, head, constructors }) =
  let
    printedCtorsArray = map (align <<< printDataCtor) constructors

    printedCtors = align $ group $ concatWith (surroundOmittingEmpty line) $ Array.zipWith (<+>) ([text "="] <> Array.replicate (Array.length constructors - 1) (text "|")) printedCtorsArray
  in printMaybeComments comments (group $ vcat [ printDataHead (text "data") head, flatAlt (text "  ") (text " ") <> printedCtors ])
printDeclaration (DeclType { comments, head, type_ }) =
  let
    doWrap :: Type -> Boolean
    doWrap (TypeForall _ _) = true
    doWrap (TypeArr _ _) = true
    doWrap (TypeOp _ _ _) = true
    doWrap (TypeConstrained _ _) = true
    doWrap _ = false

    printedType :: Doc String
    printedType = maybeWrapInParentheses (doWrap type_) $ printType $ type_
  in printMaybeComments comments (printDataHead (text "type") head <+> text "=" <+> printedType)
printDeclaration (DeclNewtype { comments, head, name, type_ }) =
  let
    doWrap :: Type -> Boolean
    doWrap (TypeApp _ _) = true
    doWrap (TypeForall _ _) = true
    doWrap (TypeArr _ _) = true
    doWrap (TypeOp _ _ _) = true
    doWrap (TypeConstrained _ _) = true
    doWrap _ = false

    printedType :: Doc String
    printedType = maybeWrapInParentheses (doWrap type_) $ printType $ type_
  in printMaybeComments comments (printDataHead (text "newtype") head <+> text "=" <+> ((text <<< appendUnderscoreIfReserved <<< unwrap) name <+> printedType))
printDeclaration (DeclFixity { comments, fixityFields: { keyword, precedence, operator } }) =
  let
    printFixityOp :: FixityOp -> Doc String
    printFixityOp (FixityValue (Left qualifiedIdent) opName) = printQualifiedName_Ident qualifiedIdent <+> text "as" <+> (text <<< appendUnderscoreIfReserved <<< unwrap) opName
    printFixityOp (FixityValue (Right qualifiedPropName) opName) = printQualifiedName_AnyProperNameType qualifiedPropName <+> text "as" <+> (text <<< appendUnderscoreIfReserved <<< unwrap) opName
    printFixityOp (FixityType qualifiedPropName opName) = text "type" <+> printQualifiedName_AnyProperNameType qualifiedPropName <+> text "as" <+> (text <<< appendUnderscoreIfReserved <<< unwrap) opName
  in printMaybeComments comments (printFixity keyword <+> text (show precedence) <+> printFixityOp operator)
printDeclaration (DeclForeign { comments, foreign_ }) =
  printMaybeComments comments
    ( text "foreign" <+> text "import" <+>
        case foreign_ of
           (ForeignValue { ident, type_ }) -> (text <<< appendUnderscoreIfReserved <<< unwrap) ident <+> text "::" <+> printType type_
           (ForeignData { name, kind_ }) -> text "data" <+> (text <<< appendUnderscoreIfReserved <<< unwrap) name <+> text "::" <+> printKind kind_
           (ForeignKind { name }) -> text "kind" <+> (text <<< appendUnderscoreIfReserved <<< unwrap) name
    )
printDeclaration (DeclDerive { comments, deriveType, head: { instName, instConstraints, instClass, instTypes } }) =
  let
    doWrap :: Type -> Boolean
    doWrap (TypeApp _ _) = true
    doWrap (TypeForall _ _) = true
    doWrap (TypeArr _ _) = true
    doWrap (TypeOp _ _ _) = true
    doWrap (TypeConstrained _ _) = true
    doWrap _ = false

    deriveType' =
      case deriveType of
        DeclDeriveType_Newtype -> text "newtype"
        DeclDeriveType_Odrinary -> emptyDoc

    constraints' =
      case instConstraints of
        [] -> emptyDoc
        [constraint] -> printConstraint constraint <+> text "=>"
        constrainsts -> (align $ group $ encloseSep (text "(") (text ")") (text ", ") $ map printConstraint constrainsts) <+> text "=>"

    types' = concatWithNonEmpty (surround space) $ map (\type_ -> maybeWrapInParentheses (doWrap type_) $ printType type_) instTypes
   in
    printMaybeComments comments $ concatWith (surroundOmittingEmpty space)
      [ text "derive"
      , deriveType'
      , text "instance"
      , text $ appendUnderscoreIfReserved $ unwrap instName
      , text "::"
      , constraints'
      , printQualifiedName_AnyProperNameType instClass
      , types'
      ]
printDeclaration (DeclClass { comments, head: { name, vars, super, fundeps }, methods }) =
  let
    printedHeader = concatWith (surroundOmittingEmpty space)
      [ text "class"
      , case super of
             [] -> emptyDoc
             [constraint] -> printConstraint constraint <+> text "<="
             constraints -> (align $ group $ encloseSep (text "(") (text ")") (text ", ") $ map printConstraint constraints) <+> text "<="
      , (text <<< appendUnderscoreIfReserved <<< unwrap) name
      , case vars of
             [] -> emptyDoc
             _ -> align $ group $ concatWith (surroundOmittingEmpty line) $ map printTypeVarBinding vars
      , case fundeps of
             [] -> emptyDoc
             _ -> text "|" <+> (align $ group $ concatWith (surroundOmittingEmpty (text ", ")) $ map printFundep $ fundeps)
      ]
   in
    if null methods
      then printMaybeComments comments printedHeader
      else
        printMaybeComments comments
          ( printedHeader <+> (text "where") <> hardline <>
              ( indent 2
              $ vcatOmittingEmpty
              $ map (\({ ident, type_ }) -> (text <<< appendUnderscoreIfReserved <<< unwrap) ident <+> text "::" <+> printType type_) $ methods
              )
          )
printDeclaration (DeclInstanceChain { comments, instances }) = printMaybeComments comments (concatWithNonEmpty (surroundOmittingEmpty (hardline <> hardline <> text "else" <> hardline <> hardline)) (map printInstance instances))
printDeclaration (DeclSignature { comments, ident, type_ }) = printMaybeComments comments ((text <<< appendUnderscoreIfReserved <<< unwrap) ident <+> text "::" <+> printType type_)
printDeclaration (DeclValue { comments, valueBindingFields }) = printMaybeComments comments (printValueBindingFields valueBindingFields)

printInstance :: Instance -> Doc String
printInstance instance_ =
  let
    head = text "instance" <+> (text <<< appendUnderscoreIfReserved <<< unwrap) instance_.head.instName <+> text "::" <+> printQualifiedName_AnyProperNameType instance_.head.instClass

    doWrap :: Type -> Boolean
    doWrap (TypeApp _ _) = true
    doWrap (TypeForall _ _) = true
    doWrap (TypeArr _ _) = true
    doWrap (TypeOp _ _ _) = true
    doWrap (TypeConstrained _ _) = true
    doWrap _ = false

    tail =
      if null instance_.head.instTypes
        then emptyDoc
        else concatWithNonEmpty (surround line) $ map (\type_ -> maybeWrapInParentheses (doWrap type_) (printType type_)) instance_.head.instTypes

    firstRow = group $ concatWith (surround line) [head, tail]
   in
    if null instance_.body
      then firstRow
      else
      let
        printedBody = printAndConditionallyAddNewlinesBetween shouldBeNoNewlineBetweenInstanceBindings printInstanceBinding instance_.body
       in firstRow <+> text "where" <> line <> indent 2 printedBody

printInstanceBinding :: InstanceBinding -> Doc String
printInstanceBinding (InstanceBindingSignature { ident, type_ }) = (text <<< appendUnderscoreIfReserved <<< unwrap) ident <+> text "::" <+> printType type_
printInstanceBinding (InstanceBindingName valueBindingFields) = printValueBindingFields valueBindingFields

printValueBindingFields :: ValueBindingFields -> Doc String
printValueBindingFields { name, binders, guarded } =
  let
    printedBinders =
      if null binders
        then emptyDoc
        else (vsep $ map printBinder binders) <> space

    printedHead = (text <<< appendUnderscoreIfReserved <<< unwrap) name <+> printedBinders <> text "="
   in printGuarded printedHead guarded

printGuarded :: Doc String -> Guarded -> Doc String
printGuarded printedHead guarded =
  case guarded of
    (Unconditional where_) ->
      case where_ of
        { expr, whereBindings: [] } ->
          if exprShouldBeOnNextLine expr
            then printedHead <> line <>(indent 2 $ printExpr expr)
            else printedHead <+> printExpr expr
        { expr, whereBindings } ->
          let
            printedBindings = indent 2 (text "where" <> line <>(printAndConditionallyAddNewlinesBetween shouldBeNoNewlineBetweenLetBindings printLetBinding whereBindings))
          in
            if exprShouldBeOnNextLine expr
              then printedHead <> line <> (indent 2 $ printExpr expr) <> line <>printedBindings
              else printedHead <+> printExpr expr <> line <>printedBindings
    (Guarded _) -> emptyDoc -- TODO

printBinder :: Binder -> Doc String
printBinder BinderWildcard = text "_"
printBinder (BinderVar ident) = (text <<< appendUnderscoreIfReserved <<< unwrap) ident
printBinder (BinderNamed { ident, binder }) = (text <<< appendUnderscoreIfReserved <<< unwrap) ident <> text "@" <> (parens $ printBinder binder)
printBinder (BinderConstructor { name, args: [] }) = printQualifiedName_AnyProperNameType name
printBinder (BinderConstructor { name, args }) = printQualifiedName_AnyProperNameType name <+> (vsep $ map printBinder args)
printBinder (BinderBoolean boolean) = text $ show boolean
printBinder (BinderChar char) = text $ show char
printBinder (BinderString string) = text $ show string
printBinder (BinderNumber (Left int)) = text $ show int
printBinder (BinderNumber (Right number)) = text $ show number
printBinder (BinderArray binders) = text "[" <> (concatWith (surroundOmittingEmpty (text ", ")) $ map printBinder binders) <> text "]"
printBinder (BinderRecord arrayRecordLabeledBinder) = concatWith (surroundOmittingEmpty (text ", ")) $ map (printRecordLabeled printBinder) arrayRecordLabeledBinder
printBinder (BinderTyped binder type_) = printBinder binder <+> text "::" <+> printType type_
printBinder (BinderOp binderLeft operator binderRight) = printBinder binderLeft <+> printQualifiedName_AnyOpNameType operator <+> printBinder binderRight

printRecordLabeled :: ∀ a . (a -> Doc String) -> RecordLabeled a -> Doc String
printRecordLabeled _ (RecordPun ident) = (text <<< quoteIfReserved <<< unwrap) ident
printRecordLabeled print (RecordField label a) = (text <<< quoteIfReserved <<< unwrap) label <> text ":" <+> print a

printExpr :: Expr -> Doc String
printExpr =
  let
    processTopLevel printExprImplementation' expr =
        case expr of
             (ExprApp _ _) -> align $ group $ printExprImplementation' expr
             (ExprArray _) -> group $ printExprImplementation' expr
             (ExprRecord _) -> group $ printExprImplementation' expr
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
    printExprImplementation (ExprArray array) = align $ encloseSep (text "[") (text "]") (text ", ") (map (processTopLevel printExprImplementation) array)
    printExprImplementation (ExprRecord arrayRecordLabeled) = text "{" <+> (concatWith (surroundOmittingEmpty (text ", ")) $ map (printRecordLabeled printExprImplementation) arrayRecordLabeled) <+> text "}"
    printExprImplementation (ExprTyped expr type_) = printExprImplementation expr <+> text "::" <+> printType type_
    printExprImplementation (ExprInfix exprLeft operator exprRight) = printExprImplementation exprLeft <+> printExprImplementation operator <+> printExprImplementation exprRight
    printExprImplementation (ExprOp exprLeft operator exprRight) = printExprImplementation exprLeft <+> printQualifiedName_AnyOpNameType operator <+> printExprImplementation exprRight
    printExprImplementation (ExprOpName opName) = printQualifiedName_AnyOpNameType opName
    printExprImplementation (ExprNegate expr) = text "-" <> printExprImplementation expr
    printExprImplementation (ExprRecordAccessor { recExpr, recPath }) = printExprImplementation recExpr <> text "." <> (concatWithNonEmpty (surround dot) $ map (text <<< appendUnderscoreIfReserved <<< unwrap) recPath)
    printExprImplementation (ExprRecordUpdate expr recordUpdates) = parens $ printExprImplementation expr <+> printRecordUpdates recordUpdates
    printExprImplementation (ExprApp exprLeft exprRight) =
      let
        doWrapRight =
          case exprRight of
            (ExprApp _ _) -> true -- always wrap right side application
            (ExprInfix _ _ _) -> true
            (ExprOp _ _ _) -> true
            _ -> false
      in concatWith (surround line) $ [ printExprImplementation exprLeft, maybeWrapInParentheses doWrapRight (printExprImplementation exprRight) ]
    printExprImplementation (ExprLambda { binders, body }) = (parens $ vsep $ map printBinder binders) <+> text "=" <+> printExprImplementation body
    printExprImplementation (ExprIf { cond, true_, false_ }) = concatWith (surround line)
      [ if exprShouldBeOnNextLine cond
          then text "if" <> hardline <> (indent 2 $ printExprImplementation cond)
          else text "if" <+> printExprImplementation cond
      , if exprShouldBeOnNextLine true_
          then indent 2 $ text "then" <> hardline <> (indent 2 $ printExprImplementation true_)
          else indent 2 $ text "then" <+> printExprImplementation true_
      , if exprShouldBeOnNextLine false_
          then indent 2 $ text "else" <> hardline <> (indent 2 $ printExprImplementation false_)
          else indent 2 $ text "else" <+> printExprImplementation false_
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

        headDocs :: Array (Doc String)
        headDocs = NonEmptyArray.toArray $ map printExprImplementation head
      in
        if headShouldBeMultiline
          then concatWith (surroundOmittingEmpty line)
            [ text "case"
            , concatWith (surroundOmittingEmpty line') $
              Array.zipWith
              (<>)
              ([text "  "] <> Array.replicate (Array.length headDocs - 1) (text ", "))
              (map align headDocs)
            , text "of"
            , indent 2 $ vcatOmittingEmptyNonEmpty $ map printBranch branches
            ]
          else
            text "case" <+> (concatWith (surround (text ", ")) headDocs) <+> text "of"
            <> hardline <>
            (indent 2 $ vcatOmittingEmptyNonEmpty $ map printBranch branches)
    printExprImplementation (ExprLet { bindings, body }) = align $ concatWith (surroundOmittingEmpty hardline)
      [ text "let"
      , indent 2 (printAndConditionallyAddNewlinesBetween shouldBeNoNewlineBetweenLetBindings printLetBinding bindings)
      , text " in"
      , indent 2 (processTopLevel printExprImplementation body)
      ]
    printExprImplementation (ExprDo doStatements) = emptyDoc -- TODO
    printExprImplementation (ExprAdo { statements, result }) = emptyDoc -- TODO
  in processTopLevel printExprImplementation

printBranch :: { binders :: NonEmptyArray Binder, body :: Guarded } -> Doc String
printBranch { binders, body } =
  let
    printedHead = (concatWithNonEmpty (surround (text ", ")) $ map printBinder binders) <+> text "->"
   in printGuarded printedHead body

printLetBinding :: LetBinding -> Doc String
printLetBinding (LetBindingSignature { ident, type_ }) = (text <<< appendUnderscoreIfReserved <<< unwrap) ident <+> text "::" <+> printType type_
printLetBinding (LetBindingName valueBindingFields) = printValueBindingFields valueBindingFields
printLetBinding (LetBindingPattern { binder, where_: { expr, whereBindings } }) = printBinder binder <> hardline <> printExpr expr <> line <>text "where" <> line <>(vsep $ map printLetBinding whereBindings)

printRecordUpdates :: NonEmptyArray RecordUpdate -> Doc String
printRecordUpdates recordUpdates = text "{" <+> (concatWithNonEmpty (surround (text ",")) $ map printRecordUpdate recordUpdates) <+> text "}"

printRecordUpdate :: RecordUpdate -> Doc String
printRecordUpdate (RecordUpdateLeaf label expr) = (text <<< appendUnderscoreIfReserved <<< unwrap) label <+> text "=" <+> printExpr expr
printRecordUpdate (RecordUpdateBranch label recordUpdates) = (text <<< appendUnderscoreIfReserved <<< unwrap) label <+> text "=" <+> printRecordUpdates recordUpdates
