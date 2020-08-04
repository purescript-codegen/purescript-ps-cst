module Language.PS.CST.Printers where

import Language.PS.CST.Printers.Utils
import Prelude
import Text.Pretty
import Text.Pretty.Symbols.String hiding (space)

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Foldable (any, intercalate, null)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (fromFoldable) as List
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap)
import Data.NonEmpty (NonEmpty(..))
import Debug.Trace (spy)
import Language.PS.CST.Printers.PrintImports (printImports)
import Language.PS.CST.Printers.PrintModuleModuleNameAndExports (printModuleModuleNameAndExports)
import Language.PS.CST.Printers.TypeLevel (printConstraint, printDataCtor, printDataHead, printFixity, printFundep, printKind, printQualifiedName_AnyOpNameType, printQualifiedName_AnyProperNameType, printQualifiedName_Ident, printType, printTypeVarBinding)
import Language.PS.CST.ReservedNames (appendUnderscoreIfReserved, quoteIfReserved)
import Language.PS.CST.Types.Declaration (Binder(..), Declaration(..), Expr(..), FixityOp(..), Foreign(..), Guarded(..), Instance, InstanceBinding(..), LetBinding(..), RecordUpdate(..), Type(..), ValueBindingFields)
import Language.PS.CST.Types.Leafs (Comments(..), DeclDeriveType(..), RecordLabeled(..))
import Language.PS.CST.Types.Module (Module(..))
import Text.Pretty as Pretty
import Text.Pretty.Code.Purescript (encloseSep)

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
printExpr (ExprHole hole) = text "?" <> (text <<< appendUnderscoreIfReserved <<< unwrap) hole
printExpr ExprSection = text "_"
printExpr (ExprIdent qualifiedIdent) = printQualifiedName_Ident qualifiedIdent
printExpr (ExprConstructor qualifiedPropName) = printQualifiedName_AnyProperNameType qualifiedPropName
printExpr (ExprBoolean boolean) = text $ show boolean
printExpr (ExprChar char) = text $ show char
printExpr (ExprString string) = text $ show string
printExpr (ExprNumber (Left int)) = text $ show int
printExpr (ExprNumber (Right num)) = text $ show num
printExpr (ExprArray array) = text "[" <> (concatWith (surroundOmittingEmpty (text ", ")) $ map printExpr array) <> text "]"
printExpr (ExprRecord arrayRecordLabeled) = text "{" <+> (concatWith (surroundOmittingEmpty (text ", ")) $ map (printRecordLabeled printExpr) arrayRecordLabeled) <+> text "}"
printExpr (ExprTyped expr type_) = printExpr expr <+> text "::" <+> printType type_
printExpr (ExprInfix exprLeft operator exprRight) = printExpr exprLeft <+> printExpr operator <+> printExpr exprRight
printExpr (ExprOp exprLeft operator exprRight) = printExpr exprLeft <+> printQualifiedName_AnyOpNameType operator <+> printExpr exprRight
printExpr (ExprOpName opName) = printQualifiedName_AnyOpNameType opName
printExpr (ExprNegate expr) = text "-" <> printExpr expr
printExpr (ExprRecordAccessor { recExpr, recPath }) = printExpr recExpr <> text "." <> (concatWithNonEmpty (surround dot) $ map (text <<< appendUnderscoreIfReserved <<< unwrap) recPath)
printExpr (ExprRecordUpdate expr recordUpdates) = parens $ printExpr expr <+> printRecordUpdates recordUpdates
printExpr (ExprApp exprLeft exprRight) =
  let
    doWrapRight =
      case exprRight of
        (ExprApp _ _) -> true -- always wrap right side application
        (ExprInfix _ _ _) -> true
        (ExprOp _ _ _) -> true
        _ -> false

    printedLeft :: Doc String
    printedLeft = printExpr exprLeft

    printedRight :: Doc String
    printedRight = printExpr exprRight

    printed :: Doc String
    printed = printedLeft <+> maybeWrapInParentheses doWrapRight printedRight
  in
    printed
printExpr (ExprLambda { binders, body }) = (parens $ vsep $ map printBinder binders) <+> text "=" <+> printExpr body
printExpr (ExprIf { cond, true_, false_ }) =
  let
    printedCond =
      if exprShouldBeOnNextLine cond
        then text "if" <> hardline <> (indent 2 $ printExpr cond)
        else text "if" <+> printExpr cond

    printedTrue =
      if exprShouldBeOnNextLine true_
        then indent 2 $ (text "then" <> hardline <> (indent 2 $ printExpr true_))
        else indent 2 $ text "then" <+> printExpr true_

    printedFalse =
      if exprShouldBeOnNextLine false_
        then indent 2 $ (text "else" <> hardline <>(indent 2 $ printExpr false_))
        else indent 2 $ text "else" <+> printExpr false_
   in
    printedCond
    <> line <>printedTrue
    <> line <>printedFalse
printExpr (ExprCase { head, branches }) =
  let
    printBranch :: { binders :: NonEmptyArray Binder, body :: Guarded } -> Doc String
    printBranch { binders, body } =
      let
        printedHead = (concatWithNonEmpty (surround (text ", ")) $ map printBinder binders) <+> text "->"
       in printGuarded printedHead body

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
    headDocs = NonEmptyArray.toArray $ map printExpr head
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
printExpr (ExprLet { bindings, body }) =
  let
    printedBindings = printAndConditionallyAddNewlinesBetween shouldBeNoNewlineBetweenLetBindings printLetBinding bindings

    printedBody = printExpr body

    printed =
      align $ concatWith (surroundOmittingEmpty hardline)
      [ text "let"
      , indent 2 printedBindings
      , text " in"
      , indent 2 printedBody
      ]
   in
    printed
printExpr (ExprDo doStatements) = emptyDoc -- TODO
printExpr (ExprAdo { statements, result }) = emptyDoc -- TODO

printLetBinding :: LetBinding -> Doc String
printLetBinding (LetBindingSignature { ident, type_ }) = (text <<< appendUnderscoreIfReserved <<< unwrap) ident <+> text "::" <+> printType type_
printLetBinding (LetBindingName valueBindingFields) = printValueBindingFields valueBindingFields
printLetBinding (LetBindingPattern { binder, where_: { expr, whereBindings } }) = printBinder binder <> hardline <> printExpr expr <> line <>text "where" <> line <>(vsep $ map printLetBinding whereBindings)

printRecordUpdates :: NonEmptyArray RecordUpdate -> Doc String
printRecordUpdates recordUpdates = text "{" <+> (concatWithNonEmpty (surround (text ",")) $ map printRecordUpdate recordUpdates) <+> text "}"

printRecordUpdate :: RecordUpdate -> Doc String
printRecordUpdate (RecordUpdateLeaf label expr) = (text <<< appendUnderscoreIfReserved <<< unwrap) label <+> text "=" <+> printExpr expr
printRecordUpdate (RecordUpdateBranch label recordUpdates) = (text <<< appendUnderscoreIfReserved <<< unwrap) label <+> text "=" <+> printRecordUpdates recordUpdates
