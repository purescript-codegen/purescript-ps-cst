module Language.PS.CST.Printers where

import Prelude

import Language.PS.CST.Printers.PrintImports (printImports)
import Language.PS.CST.Printers.PrintModuleModuleNameAndExports (printModuleModuleNameAndExports)
import Language.PS.CST.Printers.TypeLevel (PrintType_Style(..), printConstraint, printDataCtor, printDataHead, printFixity, printFundep, printKind, printQualifiedName_AnyOpNameType, printQualifiedName_AnyProperNameType, printQualifiedName_Ident, printType, printTypeVarBinding)
import Language.PS.CST.Printers.Utils (emptyColumn, emptyRow, ifelse, lines, maybeWrapInParentheses, printAndConditionallyAddNewlinesBetween, twoSpaceIdentation, wrapInParentheses)
import Language.PS.CST.Types (Binder(..), Comments(..), DeclDeriveType(..), Declaration(..), Expr(..), FixityOp(..), Foreign(..), Guarded(..), Instance, InstanceBinding(..), LetBinding(..), Module(..), RecordLabeled(..), RecordUpdate(..), Type(..), ValueBindingFields)
import Language.PS.CST.ReservedNames (appendUnderscoreIfReserved, quoteIfReserved)

import Data.Newtype (unwrap)
import Data.Either (Either(..))
import Data.Foldable (any, null)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (fromFoldable) as List
import Data.Maybe (Maybe, maybe)
import Data.Array.NonEmpty (NonEmptyArray)
import Text.PrettyPrint.Boxes (Box, left, nullBox, punctuateH, punctuateV, text, vcat, vsep, (/+/), (//), (<<+>>), (<<>>))
import Text.PrettyPrint.Boxes (render) as Text.PrettyPrint.Boxes

printModuleToString :: Module -> String
printModuleToString = Text.PrettyPrint.Boxes.render <<< printModule

printModule :: Module -> Box
printModule (Module { moduleName, imports, exports, declarations }) =
  lines
    $ [ printModuleModuleNameAndExports moduleName exports
      , printImports imports
      , printDeclarations declarations
      , emptyRow
      ]

shouldBeNoNewlineBetweenDeclarations :: Declaration -> Declaration -> Boolean
shouldBeNoNewlineBetweenDeclarations (DeclSignature { ident }) (DeclValue { valueBindingFields: { name } }) = ident == name
shouldBeNoNewlineBetweenDeclarations (DeclValue { valueBindingFields: { name } }) (DeclValue { valueBindingFields: { name: nameNext } }) = name == nameNext
shouldBeNoNewlineBetweenDeclarations _ _ = false

shouldBeNoNewlineBetweenLetBindings :: LetBinding -> LetBinding -> Boolean
shouldBeNoNewlineBetweenLetBindings (LetBindingSignature { ident }) (LetBindingName { name }) = ident == name
shouldBeNoNewlineBetweenLetBindings (LetBindingName { name }) (LetBindingName { name: nameNext }) = name == nameNext
shouldBeNoNewlineBetweenLetBindings _ _ = false

shouldBeNoNewlineBetweenInstanceBindings :: InstanceBinding -> InstanceBinding -> Boolean
shouldBeNoNewlineBetweenInstanceBindings (InstanceBindingSignature { ident }) (InstanceBindingName { name }) = ident == name
shouldBeNoNewlineBetweenInstanceBindings (InstanceBindingName { name }) (InstanceBindingName { name: nameNext }) = name == nameNext
shouldBeNoNewlineBetweenInstanceBindings _ _ = false

printDeclarations :: Array Declaration -> Box
printDeclarations [] = nullBox
printDeclarations declarations = emptyRow // printAndConditionallyAddNewlinesBetween shouldBeNoNewlineBetweenDeclarations printDeclaration declarations

printComments :: Comments -> Box
printComments (OneLineComments strings) = strings <#> (\x -> text "-- |" <<+>> text x) # vcat left
printComments (BlockComments strings) = text "{-" // (strings <#> (\x -> twoSpaceIdentation <<>> text x) # vcat left) // text "-}"

printMaybeComments :: Maybe Comments -> Box
printMaybeComments = maybe nullBox printComments

printDeclaration :: Declaration -> Box
printDeclaration (DeclData { comments, head, constructors: [] }) = printMaybeComments comments // printDataHead (text "data") head
printDeclaration (DeclData { comments, head, constructors }) =
  let
    printedCtors =
      constructors
        <#> printDataCtor
        # mapWithIndex (\i box -> ifelse (i == 0) (text "=") (text "|") <<+>> box)
        <#> (twoSpaceIdentation <<>> _)
        # vcat left
  in
    printMaybeComments comments // printDataHead (text "data") head // printedCtors
printDeclaration (DeclType { comments, head, type_ }) =
  let
    doWrap :: Type -> Boolean
    doWrap (TypeForall _ _) = true
    doWrap (TypeArr _ _) = true
    doWrap (TypeOp _ _ _) = true
    doWrap (TypeConstrained _ _) = true
    doWrap _ = false

    printedType :: Box
    printedType = maybeWrapInParentheses (doWrap type_) $ printType PrintType_Multiline $ type_
  in
    printMaybeComments comments // (printDataHead (text "type") head <<+>> text "=" <<+>> printedType)
printDeclaration (DeclNewtype { comments, head, name, type_ }) =
  let
    doWrap :: Type -> Boolean
    doWrap (TypeApp _ _) = true
    doWrap (TypeForall _ _) = true
    doWrap (TypeArr _ _) = true
    doWrap (TypeOp _ _ _) = true
    doWrap (TypeConstrained _ _) = true
    doWrap _ = false

    printedType :: Box
    printedType = maybeWrapInParentheses (doWrap type_) $ printType PrintType_Multiline $ type_
  in
    printMaybeComments comments // (printDataHead (text "newtype") head <<+>> text "=" <<+>> ((text <<< appendUnderscoreIfReserved <<< unwrap) name <<+>> printedType))
printDeclaration (DeclFixity { comments, fixityFields: { keyword, precedence, operator } }) =
  let
    printFixityOp :: FixityOp -> Box
    printFixityOp (FixityValue (Left qualifiedIdent) opName) = printQualifiedName_Ident qualifiedIdent <<+>> text "as" <<+>> (text <<< appendUnderscoreIfReserved <<< unwrap) opName
    printFixityOp (FixityValue (Right qualifiedPropName) opName) = printQualifiedName_AnyProperNameType qualifiedPropName <<+>> text "as" <<+>> (text <<< appendUnderscoreIfReserved <<< unwrap) opName
    printFixityOp (FixityType qualifiedPropName opName) = text "type" <<+>> printQualifiedName_AnyProperNameType qualifiedPropName <<+>> text "as" <<+>> (text <<< appendUnderscoreIfReserved <<< unwrap) opName
  in
    printMaybeComments comments // (printFixity keyword <<+>> text (show precedence) <<+>> printFixityOp operator)
printDeclaration (DeclForeign { comments, foreign_ }) =
  printMaybeComments comments //
    ( text "foreign" <<+>> text "import" <<+>>
        case foreign_ of
           (ForeignValue { ident, type_ }) -> (text <<< appendUnderscoreIfReserved <<< unwrap) ident <<+>> text "::" <<+>> printType PrintType_Multiline type_
           (ForeignData { name, kind_ }) -> text "data" <<+>> (text <<< appendUnderscoreIfReserved <<< unwrap) name <<+>> text "::" <<+>> printKind kind_
           (ForeignKind { name }) -> text "kind" <<+>> (text <<< appendUnderscoreIfReserved <<< unwrap) name
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
        DeclDeriveType_Newtype -> emptyColumn <<>> text "newtype"
        DeclDeriveType_Odrinary -> nullBox

    constraints' =
      case instConstraints of
        [] -> nullBox
        [constraint] -> emptyColumn <<>> printConstraint constraint <<+>> text "=>"
        constrainsts -> emptyColumn <<>> (wrapInParentheses $ punctuateH left (text ", ") $ map printConstraint constrainsts) <<+>> text "=>"

    types' = punctuateH left (emptyColumn) $ map (\type_ -> maybeWrapInParentheses (doWrap type_) $ printType PrintType_OneLine type_) instTypes
   in
   printMaybeComments comments // (text "derive" <<>> deriveType' <<+>> text "instance" <<+>> (text <<< appendUnderscoreIfReserved <<< unwrap) instName <<+>> text "::" <<>> constraints' <<+>> printQualifiedName_AnyProperNameType instClass <<+>> types')
printDeclaration (DeclClass { comments, head: { name, vars, super, fundeps }, methods }) =
  let
    printedVars =
      if null vars
        then nullBox
        else emptyColumn <<>> (punctuateH left emptyColumn $ map printTypeVarBinding vars)

    printedSuper =
      case super of
        [] -> nullBox
        [constraint] -> printConstraint constraint <<+>> text "<=" <<>> emptyColumn
        constraints -> (wrapInParentheses $ punctuateH left (text ", ") $ map printConstraint constraints) <<+>> text "<=" <<>> emptyColumn

    printedFundeps =
      if null fundeps
        then nullBox
        else emptyColumn <<>> text "|" <<+>> (fundeps # map printFundep # punctuateH left (text ", "))

    printedHeader = text "class" <<+>> printedSuper <<>> (text <<< appendUnderscoreIfReserved <<< unwrap) name <<>> printedVars <<>> printedFundeps
   in
    if null methods
      then printMaybeComments comments // printedHeader
      else
        printMaybeComments comments //
          ( printedHeader <<+>> (text "where")
            // (methods
                <#> (\({ ident, type_ }) -> (text <<< appendUnderscoreIfReserved <<< unwrap) ident <<+>> text "::" <<+>> (printType PrintType_Multiline type_))
                <#> (twoSpaceIdentation <<>> _)
                # vcat left
                )
          )
printDeclaration (DeclInstanceChain { comments, instances }) =
  let
    printInstance :: Instance -> Box
    printInstance { head: { instName, instConstraints, instClass, instTypes }, body } =
      let
        head = text "instance" <<+>> (text <<< appendUnderscoreIfReserved <<< unwrap) instName <<+>> text "::" <<+>> printQualifiedName_AnyProperNameType instClass

        doWrap :: Type -> Boolean
        doWrap (TypeApp _ _) = true
        doWrap (TypeForall _ _) = true
        doWrap (TypeArr _ _) = true
        doWrap (TypeOp _ _ _) = true
        doWrap (TypeConstrained _ _) = true
        doWrap _ = false

        tail =
          if null instTypes
            then nullBox
            else emptyColumn <<>> (instTypes <#> (\type_ -> maybeWrapInParentheses (doWrap type_) (printType PrintType_OneLine type_)) # punctuateH left emptyColumn)

        firstRow = head <<>> tail
       in
        if null body
          then printMaybeComments comments // firstRow
          else
          let
            printedBody = printAndConditionallyAddNewlinesBetween shouldBeNoNewlineBetweenInstanceBindings printInstanceBinding body
           in
            printMaybeComments comments
            // (firstRow <<+>> text "where")
            // (twoSpaceIdentation <<>> printedBody)
   in instances <#> printInstance # punctuateV left (nullBox /+/ text "else" /+/ nullBox)
printDeclaration (DeclSignature { comments, ident, type_ }) = printMaybeComments comments // ((text <<< appendUnderscoreIfReserved <<< unwrap) ident <<+>> text "::" <<+>> printType PrintType_Multiline type_)
printDeclaration (DeclValue { comments, valueBindingFields }) = printMaybeComments comments // (printValueBindingFields valueBindingFields)

printInstanceBinding :: InstanceBinding -> Box
printInstanceBinding (InstanceBindingSignature { ident, type_ }) = (text <<< appendUnderscoreIfReserved <<< unwrap) ident <<+>> text "::" <<+>> printType PrintType_Multiline type_
printInstanceBinding (InstanceBindingName valueBindingFields) = printValueBindingFields valueBindingFields

printValueBindingFields :: ValueBindingFields -> Box
printValueBindingFields { name, binders, guarded } =
  let
    printedBinders =
      if null binders
        then nullBox
        else (punctuateH left emptyColumn $ map printBinder binders) <<>> emptyColumn

    printedHead = (text <<< appendUnderscoreIfReserved <<< unwrap) name <<+>> printedBinders <<>> text "="
   in printGuarded printedHead guarded

printGuarded :: Box -> Guarded -> Box
printGuarded printedHead guarded =
  case guarded of
    (Unconditional where_) ->
      case where_ of
        { expr, whereBindings: [] } ->
          if exprShouldBeOnNextLine expr
            then printedHead // (twoSpaceIdentation <<>> printExpr expr)
            else printedHead <<+>> printExpr expr
        { expr, whereBindings } ->
          let
            printedBindings = twoSpaceIdentation <<>> (text "where" // (printAndConditionallyAddNewlinesBetween shouldBeNoNewlineBetweenLetBindings printLetBinding whereBindings))
          in
            if exprShouldBeOnNextLine expr
              then printedHead // (twoSpaceIdentation <<>> printExpr expr) // printedBindings
              else printedHead <<+>> printExpr expr // printedBindings
    (Guarded _) -> nullBox -- TODO

exprShouldBeOnNextLine :: Expr -> Boolean
exprShouldBeOnNextLine (ExprLet _) = true
exprShouldBeOnNextLine (ExprCase _) = true
exprShouldBeOnNextLine (ExprIf _) = true
exprShouldBeOnNextLine _ = false

printBinder :: Binder -> Box
printBinder BinderWildcard = text "_"
printBinder (BinderVar ident) = (text <<< appendUnderscoreIfReserved <<< unwrap) ident
printBinder (BinderNamed { ident, binder }) = (text <<< appendUnderscoreIfReserved <<< unwrap) ident <<>> text "@" <<>> (wrapInParentheses $ printBinder binder)
printBinder (BinderConstructor { name, args: [] }) = printQualifiedName_AnyProperNameType name
printBinder (BinderConstructor { name, args }) = printQualifiedName_AnyProperNameType name <<+>> (punctuateH left emptyColumn $ map printBinder args)
printBinder (BinderBoolean boolean) = text $ show boolean
printBinder (BinderChar char) = text $ show char
printBinder (BinderString string) = text $ show string
printBinder (BinderNumber (Left int)) = text $ show int
printBinder (BinderNumber (Right number)) = text $ show number
printBinder (BinderArray binders) = text "[" <<>> (punctuateH left (text ", ") $ map printBinder binders) <<>> text "]"
printBinder (BinderRecord arrayRecordLabeledBinder) = punctuateH left (text ", ") $ map (printRecordLabeled printBinder) arrayRecordLabeledBinder
printBinder (BinderTyped binder type_) = printBinder binder <<+>> text "::" <<+>> printType PrintType_OneLine type_
printBinder (BinderOp binderLeft operator binderRight) = printBinder binderLeft <<+>> printQualifiedName_AnyOpNameType operator <<+>> printBinder binderRight

printRecordLabeled :: ∀ a . (a -> Box) -> RecordLabeled a -> Box
printRecordLabeled _ (RecordPun ident) = (text <<< quoteIfReserved <<< unwrap) ident
printRecordLabeled print (RecordField label a) = (text <<< quoteIfReserved <<< unwrap) label <<>> text ":" <<+>> print a

printExpr :: Expr -> Box
printExpr (ExprHole hole) = text "?" <<>> (text <<< appendUnderscoreIfReserved <<< unwrap) hole
printExpr ExprSection = text "_"
printExpr (ExprIdent qualifiedIdent) = printQualifiedName_Ident qualifiedIdent
printExpr (ExprConstructor qualifiedPropName) = printQualifiedName_AnyProperNameType qualifiedPropName
printExpr (ExprBoolean boolean) = text $ show boolean
printExpr (ExprChar char) = text $ show char
printExpr (ExprString string) = text $ show string
printExpr (ExprNumber (Left int)) = text $ show int
printExpr (ExprNumber (Right num)) = text $ show num
printExpr (ExprArray array) = text "[" <<>> (punctuateH left (text ", ") $ map printExpr array) <<>> text "]"
printExpr (ExprRecord arrayRecordLabeled) = text "{" <<+>> (punctuateH left (text ", ") $ map (printRecordLabeled printExpr) arrayRecordLabeled) <<+>> text "}"
printExpr (ExprTyped expr type_) = printExpr expr <<+>> text "::" <<+>> printType PrintType_OneLine type_
printExpr (ExprInfix exprLeft operator exprRight) = printExpr exprLeft <<+>> printExpr operator <<+>> printExpr exprRight
printExpr (ExprOp exprLeft operator exprRight) = printExpr exprLeft <<+>> printQualifiedName_AnyOpNameType operator <<+>> printExpr exprRight
printExpr (ExprOpName opName) = printQualifiedName_AnyOpNameType opName
printExpr (ExprNegate expr) = text "-" <<>> printExpr expr
printExpr (ExprRecordAccessor { recExpr, recPath }) = printExpr recExpr <<>> text "." <<>> (punctuateH left (text ".") $ map (text <<< appendUnderscoreIfReserved <<< unwrap) recPath)
printExpr (ExprRecordUpdate expr recordUpdates) = wrapInParentheses $ printExpr expr <<+>> printRecordUpdates recordUpdates
printExpr (ExprApp exprLeft exprRight) =
  let
    doWrapRight =
      case exprRight of
        (ExprApp _ _) -> true -- always wrap right side application
        (ExprInfix _ _ _) -> true
        (ExprOp _ _ _) -> true
        _ -> false

    printedLeft :: Box
    printedLeft = printExpr exprLeft

    printedRight :: Box
    printedRight = printExpr exprRight

    printed :: Box
    printed = printedLeft <<+>> maybeWrapInParentheses doWrapRight printedRight
  in
    printed
printExpr (ExprLambda { binders, body }) = (wrapInParentheses $ punctuateH left emptyColumn $ map printBinder binders) <<+>> text "=" <<+>> printExpr body
printExpr (ExprIf { cond, true_, false_ }) =
  let
    printedCond =
      if exprShouldBeOnNextLine cond
        then text "if" // (twoSpaceIdentation <<>> printExpr cond)
        else text "if" <<+>> printExpr cond

    printedTrue =
      if exprShouldBeOnNextLine true_
        then twoSpaceIdentation <<>> (text "then" // (twoSpaceIdentation <<>> printExpr true_))
        else twoSpaceIdentation <<>> text "then" <<+>> printExpr true_

    printedFalse =
      if exprShouldBeOnNextLine false_
        then twoSpaceIdentation <<>> (text "else" // (twoSpaceIdentation <<>> printExpr false_))
        else twoSpaceIdentation <<>> text "else" <<+>> printExpr false_
   in
    printedCond
    // printedTrue
    // printedFalse
printExpr (ExprCase { head, branches }) =
  let
    printBranch :: { binders :: NonEmptyArray Binder, body :: Guarded } -> Box
    printBranch { binders, body } =
      let
        printedHead = (punctuateH left (text ", ") $ map printBinder binders) <<+>> text "->"
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
  in
    if headShouldBeMultiline
      then text "case"
      // (
        head
        # List.fromFoldable
        <#> printExpr
        # mapWithIndex (\i box -> ifelse (i == 0) (twoSpaceIdentation <<>> box) (text ", " <<>> box))
        # vcat left
      )
      // text "of"
      // (twoSpaceIdentation <<>> (vcat left $ map printBranch branches))
      else text "case" <<+>> (punctuateH left (text ", ") $ map printExpr head) <<+>> text "of" // (twoSpaceIdentation <<>> (vcat left $ map printBranch branches))
printExpr (ExprLet { bindings, body }) =
  let
    printedBindings = printAndConditionallyAddNewlinesBetween shouldBeNoNewlineBetweenLetBindings printLetBinding bindings

    printedBody = printExpr body

    printed =
      text "let"
      // (twoSpaceIdentation <<>> printedBindings)
      // text "in"
      // (twoSpaceIdentation <<>> printedBody)
   in
    printed
printExpr (ExprDo doStatements) = nullBox -- TODO
printExpr (ExprAdo { statements, result }) = nullBox -- TODO

printLetBinding :: LetBinding -> Box
printLetBinding (LetBindingSignature { ident, type_ }) = (text <<< appendUnderscoreIfReserved <<< unwrap) ident <<+>> text "::" <<+>> printType PrintType_Multiline type_
printLetBinding (LetBindingName valueBindingFields) = printValueBindingFields valueBindingFields
printLetBinding (LetBindingPattern { binder, where_: { expr, whereBindings } }) = printBinder binder /+/ printExpr expr // text "where" // (vsep 1 left $ map printLetBinding whereBindings)

printRecordUpdates :: NonEmptyArray RecordUpdate -> Box
printRecordUpdates recordUpdates = text "{" <<+>> (punctuateH left (text ",") $ map printRecordUpdate recordUpdates) <<+>> text "}"

printRecordUpdate :: RecordUpdate -> Box
printRecordUpdate (RecordUpdateLeaf label expr) = (text <<< appendUnderscoreIfReserved <<< unwrap) label <<+>> text "=" <<+>> printExpr expr
printRecordUpdate (RecordUpdateBranch label recordUpdates) = (text <<< appendUnderscoreIfReserved <<< unwrap) label <<+>> text "=" <<+>> printRecordUpdates recordUpdates
