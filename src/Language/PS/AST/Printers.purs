module Language.PS.AST.Printers where

import Language.PS.AST.Printers.PrintImports
import Language.PS.AST.Printers.PrintModuleModuleNameAndExports
import Language.PS.AST.Printers.TypeLevel
import Language.PS.AST.Printers.Utils
import Language.PS.AST.Types
import Prelude

import Data.Array (snoc) as Array
import Data.Either (Either(..))
import Data.Foldable (class Foldable, null)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..), (:))
import Data.List (fromFoldable) as List
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.NonEmpty (NonEmpty(..))
import Data.Variant (contract)
import Text.PrettyPrint.Boxes (Box, left, nullBox, punctuateH, text, vcat, vsep, (//), (<<+>>), (<<>>))
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

printDeclarations :: Array Declaration -> Box
printDeclarations [] = nullBox
printDeclarations declarations =
  let
    declarations' = List.fromFoldable declarations

    shouldBeNoNewlineBetweenDeclarations :: Declaration -> Declaration -> Boolean
    shouldBeNoNewlineBetweenDeclarations (DeclSignature { ident }) (DeclValue { name }) = ident == name
    shouldBeNoNewlineBetweenDeclarations (DeclValue { name }) (DeclValue { name: nameNext }) = name == nameNext
    shouldBeNoNewlineBetweenDeclarations _ _ = false

    foldDeclaration accum Nothing current = accum // printDeclaration current
    foldDeclaration accum (Just prev) current = accum //
                                                if shouldBeNoNewlineBetweenDeclarations prev current
                                                  then printDeclaration current
                                                  else emptyRow // printDeclaration current

    printedDeclarations = foldWithPrev foldDeclaration emptyRow declarations'
  in
    printedDeclarations


printDeclaration :: Declaration -> Box
printDeclaration (DeclData { head, constructors: [] }) = printDataHead (text "data") head
printDeclaration (DeclData { head, constructors }) =
  let
    printedCtors =
      constructors
        <#> printDataCtor
        # mapWithIndex (\i box -> ifelse (i == 0) (text "=") (text "|") <<+>> box)
        <#> (twoSpaceIdentation <<>> _)
        # vcat left
  in
    printDataHead (text "data") head // printedCtors
printDeclaration (DeclType { head, type_ }) =
  let
    doWrap :: Type -> Boolean
    doWrap (TypeForall _ _) = true
    doWrap (TypeArr _ _) = true
    doWrap (TypeOp _ _ _) = true
    doWrap (TypeConstrained _ _) = true
    doWrap _ = false

    printType' :: Type -> Box
    printType' type_ = maybeWrapInParentheses (doWrap type_) $ printType PrintType_Multiline $ type_
  in
    printDataHead (text "type") head <<+>> text "=" <<+>> printType' type_
printDeclaration (DeclNewtype { head, name, type_ }) =
  let
    doWrap :: Type -> Boolean
    doWrap (TypeApp _ _) = true
    doWrap (TypeForall _ _) = true
    doWrap (TypeArr _ _) = true
    doWrap (TypeOp _ _ _) = true
    doWrap (TypeConstrained _ _) = true
    doWrap _ = false

    printType' :: Type -> Box
    printType' type_ = maybeWrapInParentheses (doWrap type_) $ printType PrintType_Multiline $ type_
  in
    printDataHead (text "newtype") head <<+>> text "=" <<+>> (textFromNewtype name <<+>> printType' type_)
printDeclaration (DeclFixity { keyword, precedence, operator }) =
  let
    printFixityOp :: FixityOp -> Box
    printFixityOp (FixityValue (Left qualifiedIdent) opName) = printQualifiedName_Ident qualifiedIdent <<+>> text "as" <<+>> textFromNewtype opName
    printFixityOp (FixityValue (Right qualifiedPropName) opName) = printQualifiedName_AnyProperNameType qualifiedPropName <<+>> text "as" <<+>> textFromNewtype opName
    printFixityOp (FixityType qualifiedPropName opName) = text "type" <<+>> printQualifiedName_AnyProperNameType qualifiedPropName <<+>> text "as" <<+>> textFromNewtype opName
  in
    printFixity keyword <<+>> text (show precedence) <<+>> printFixityOp operator
printDeclaration (DeclForeign foreign_) =
  text "foreign" <<+>> text "import" <<+>>
    case foreign_ of
      (ForeignValue { ident, type_ }) -> textFromNewtype ident <<+>> text "::" <<+>> printType PrintType_Multiline type_
      (ForeignData { name, kind_ }) -> text "data" <<+>> textFromNewtype name <<+>> text "::" <<+>> printKind kind_
      (ForeignKind { name }) -> text "kind" <<+>> textFromNewtype name
printDeclaration (DeclDerive deriveType { instName, instConstraints, instClass, instTypes }) =
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
   text "derive" <<>> deriveType' <<+>> text "instance" <<+>> textFromNewtype instName <<+>> text "::" <<>> constraints' <<+>> printQualifiedName_AnyProperNameType instClass <<+>> types'
printDeclaration (DeclClass { head: { name, vars, super, fundeps }, methods }) =
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

    printedHeader = text "class" <<+>> printedSuper <<>> textFromNewtype name <<>> printedVars <<>> printedFundeps
   in
    if null methods
      then printedHeader
      else
        printedHeader <<+>> (text "where")
        // (methods
            <#> (\({ ident, type_ }) -> textFromNewtype ident <<+>> text "::" <<+>> (printType PrintType_Multiline type_))
            <#> (twoSpaceIdentation <<>> _)
            # vcat left
           )
printDeclaration (DeclInstanceChain _) = nullBox
printDeclaration (DeclSignature { ident, type_ }) = textFromNewtype ident <<+>> text "::" <<+>> printType PrintType_Multiline type_
printDeclaration (DeclValue { name, binders, guarded }) =
  let
    printedBinders =
      if null binders
        then nullBox
        else (punctuateH left (text " ") $ map printBinder binders) <<>> emptyColumn
   in textFromNewtype name <<+>> printedBinders <<>> text "=" <<+>> printGuarded guarded

printBinder :: Binder -> Box
printBinder BinderWildcard = text "_"
printBinder (BinderVar ident) = textFromNewtype ident
printBinder (BinderNamed { ident, binder }) = textFromNewtype ident <<>> text "@" <<>> (wrapInParentheses $ printBinder binder)
printBinder (BinderConstructor { name, args: [] }) = printQualifiedName_AnyProperNameType name
printBinder (BinderConstructor { name, args }) = printQualifiedName_AnyProperNameType name <<+>> (punctuateH left (text " ") $ map printBinder args)
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
printRecordLabeled _ (RecordPun ident) = textFromNewtype ident
printRecordLabeled print (RecordField label a) = textFromNewtype label <<>> text ":" <<>> print a

printGuarded :: Guarded -> Box
printGuarded (Unconditional ({ expr, bindings: [] })) = printExpr expr
printGuarded (Unconditional ({ expr, bindings })) = printExpr expr // text "where" // (vsep 1 left $ map printLetBinding bindings)
printGuarded (Guarded guardedExprs) = nullBox

printExpr :: Expr -> Box
printExpr (ExprHole hole) = text "?" <<>> textFromNewtype hole
printExpr (ExprIdent qualifiedIdent) = printQualifiedName_Ident qualifiedIdent
printExpr (ExprConstructor qualifiedPropName) = printQualifiedName_AnyProperNameType qualifiedPropName
printExpr (ExprBoolean boolean) = text $ show boolean
printExpr (ExprChar char) = text $ show char
printExpr (ExprString string) = text $ show string
printExpr (ExprNumber (Left int)) = text $ show int
printExpr (ExprNumber (Right num)) = text $ show num
printExpr (ExprArray array) = text "[" <<>> (punctuateH left (text ", ") $ map printExpr array) <<>> text "]"
printExpr (ExprRecord arrayRecordLabeled) = punctuateH left (text ", ") $ map (printRecordLabeled printExpr) arrayRecordLabeled
printExpr (ExprTyped expr type_) = printExpr expr <<+>> text "::" <<+>> printType PrintType_OneLine type_
printExpr (ExprInfix exprLeft operator exprRight) = printExpr exprLeft <<+>> printExpr operator <<+>> printExpr exprRight
printExpr (ExprOp exprLeft operator exprRight) = printExpr exprLeft <<+>> printQualifiedName_AnyOpNameType operator <<+>> printExpr exprRight
printExpr (ExprOpName opName) = printQualifiedName_AnyOpNameType opName
printExpr (ExprNegate expr) = text "-" <<>> printExpr expr -- ????
printExpr (ExprRecordAccessor { recExpr, recPath }) = printExpr recExpr <<>> text "." <<>> (punctuateH left (text ".") $ map textFromNewtype recPath)
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
printExpr (ExprLambda { binders, body }) = (wrapInParentheses $ punctuateH left (text " ") $ map printBinder binders) <<+>> text "=" <<+>> printExpr body
printExpr (ExprIf { cond, true_, false_ }) = text "if" <<+>> printExpr cond // text "then" <<+>> printExpr true_ // text "else" <<+>> printExpr false_
printExpr (ExprCase { head, branches }) =
  let
    printBranch :: { binders :: NonEmpty Array Binder, body :: Guarded } -> Box
    printBranch { binders, body } = nullBox
  in text "case" <<+>> (punctuateH left (text " ") $ map printExpr head) <<+>> text "of" // (vcat left $ map printBranch branches)
printExpr (ExprLet { bindings, body }) = nullBox
printExpr (ExprDo doStatements) = nullBox
printExpr (ExprAdo { statements, result }) = nullBox

printLetBinding :: LetBinding -> Box
printLetBinding (LetBindingSignature { ident, type_ }) = nullBox
printLetBinding (LetBindingName { name, binders, guarded }) = nullBox
printLetBinding (LetBindingPattern { binder, where_ }) = nullBox

printRecordUpdates :: NonEmpty Array RecordUpdate -> Box
printRecordUpdates recordUpdates = text "{" <<+>> (punctuateH left (text ",") $ map printRecordUpdate recordUpdates) <<+>> text "}"

printRecordUpdate :: RecordUpdate -> Box
printRecordUpdate (RecordUpdateLeaf label expr) = textFromNewtype label <<+>> text "=" <<+>> printExpr expr
printRecordUpdate (RecordUpdateBranch label recordUpdates) = textFromNewtype label <<+>> text "=" <<+>> printRecordUpdates recordUpdates
