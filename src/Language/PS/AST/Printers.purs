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

    context = { printType_Style: PrintType_Multiline, printType_IsInsideOfApp: PrintType_IsInsideOfApp_No }

    maybeWrap x = if doWrap x then wrapInParentheses else identity

    printType' :: Type -> Box
    printType' type_ = maybeWrap type_ $ printType context $ type_
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

    context = { printType_Style: PrintType_Multiline, printType_IsInsideOfApp: PrintType_IsInsideOfApp_No }

    maybeWrap x = if doWrap x then wrapInParentheses else identity

    printType' :: Type -> Box
    printType' type_ = maybeWrap type_ $ printType context $ type_
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
      (ForeignValue { ident, type_ }) -> textFromNewtype ident <<+>> text "::" <<+>> printType { printType_Style: PrintType_Multiline, printType_IsInsideOfApp: PrintType_IsInsideOfApp_No } type_
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

    maybeWrap x = if doWrap x then wrapInParentheses else identity

    deriveType' =
      case deriveType of
        DeclDeriveType_Newtype -> emptyColumn <<>> text "newtype"
        DeclDeriveType_Odrinary -> nullBox

    constraints' =
      case instConstraints of
        [] -> nullBox
        [constraint] -> emptyColumn <<>> printConstraint constraint <<+>> text "=>"
        constrainsts -> emptyColumn <<>> (wrapInParentheses $ punctuateH left (text ", ") $ map printConstraint constrainsts) <<+>> text "=>"

    types' = punctuateH left (emptyColumn) $ map (\type_ -> maybeWrap type_ $ printType (initialPrintType_Context PrintType_OneLine) type_) instTypes
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
            <#> (\({ ident, type_ }) -> textFromNewtype ident <<+>> text "::" <<+>> (printType (initialPrintType_Context PrintType_OneLine) type_))
            <#> (twoSpaceIdentation <<>> _)
            # vcat left
           )
printDeclaration (DeclInstanceChain _) = nullBox
printDeclaration (DeclSignature { ident, type_ }) = textFromNewtype ident <<+>> text "::" <<+>> printType (initialPrintType_Context PrintType_OneLine) type_
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
printBinder (BinderTyped binder type_) = printBinder binder <<+>> text "::" <<+>> printType (initialPrintType_Context PrintType_OneLine) type_
printBinder (BinderOp binderLeft operator binderRight) = printBinder binderLeft <<+>> printQualifiedName_AnyOpNameType operator <<+>> printBinder binderRight

printRecordLabeled :: ∀ a . (a -> Box) -> RecordLabeled a -> Box
printRecordLabeled _ (RecordPun ident) = textFromNewtype ident
printRecordLabeled print (RecordField label a) = textFromNewtype label <<>> text ":" <<>> print a

printGuarded :: Guarded -> Box
printGuarded (Unconditional ({ expr, bindings: [] })) = (printExpr PrintExpr_IsInsideOfApp_No) expr
printGuarded (Unconditional ({ expr, bindings })) = (printExpr PrintExpr_IsInsideOfApp_No) expr // text "where" // (vsep 1 left $ map printLetBinding bindings)
printGuarded (Guarded guardedExprs) = nullBox

-- Am I inside of ExprApp that didn't yet break? (i.e. ExprApp inside ExprApp)
-- used to prevent multiple wraps
data PrintExpr_IsInsideOfApp
  = PrintExpr_IsInsideOfApp_Yes
  | PrintExpr_IsInsideOfApp_No

printExpr :: PrintExpr_IsInsideOfApp -> Expr -> Box
printExpr _ (ExprHole hole) = text "?" <<>> textFromNewtype hole
printExpr _ (ExprIdent qualifiedIdent) = printQualifiedName_Ident qualifiedIdent
printExpr _ (ExprConstructor qualifiedPropName) = printQualifiedName_AnyProperNameType qualifiedPropName
printExpr _ (ExprBoolean boolean) = text $ show boolean
printExpr _ (ExprChar char) = text $ show char
printExpr _ (ExprString string) = text $ show string
printExpr _ (ExprNumber (Left int)) = text $ show int
printExpr _ (ExprNumber (Right num)) = text $ show num
printExpr _ (ExprArray array) = text "[" <<>> (punctuateH left (text ", ") $ map (printExpr PrintExpr_IsInsideOfApp_No) array) <<>> text "]"
printExpr _ (ExprRecord arrayRecordLabeled) = punctuateH left (text ", ") $ map (printRecordLabeled (printExpr PrintExpr_IsInsideOfApp_No)) arrayRecordLabeled
printExpr _ (ExprTyped expr type_) = (printExpr PrintExpr_IsInsideOfApp_No) expr <<+>> text "::" <<+>> printType (initialPrintType_Context PrintType_OneLine) type_
printExpr _ (ExprInfix exprLeft operator exprRight) = (printExpr PrintExpr_IsInsideOfApp_No) exprLeft <<+>> (printExpr PrintExpr_IsInsideOfApp_No) operator <<+>> (printExpr PrintExpr_IsInsideOfApp_No) exprRight
printExpr _ (ExprOp exprLeft operator exprRight) = (printExpr PrintExpr_IsInsideOfApp_No) exprLeft <<+>> printQualifiedName_AnyOpNameType operator <<+>> (printExpr PrintExpr_IsInsideOfApp_No) exprRight
printExpr _ (ExprOpName opName) = printQualifiedName_AnyOpNameType opName
printExpr _ (ExprNegate expr) = text "-" <<>> (printExpr PrintExpr_IsInsideOfApp_No) expr -- ????
printExpr _ (ExprRecordAccessor { recExpr, recPath }) = (printExpr PrintExpr_IsInsideOfApp_No) recExpr <<>> text "." <<>> (punctuateH left (text ".") $ map textFromNewtype recPath)
printExpr _ (ExprRecordUpdate expr recordUpdates) = wrapInParentheses $ (printExpr PrintExpr_IsInsideOfApp_No) expr <<+>> printRecordUpdates recordUpdates
printExpr printExpr_IsInsideOfApp (ExprApp exprLeft exprRight) =
  let
    doWrap :: Expr -> Boolean
    doWrap (ExprApp _ _) =
      case printExpr_IsInsideOfApp of
        PrintExpr_IsInsideOfApp_No -> false
        PrintExpr_IsInsideOfApp_Yes -> true
    doWrap (ExprOp _ _ _) = true
    doWrap _ = false

    newLeftContext :: PrintExpr_IsInsideOfApp
    newLeftContext = PrintExpr_IsInsideOfApp_No

    newRightContext :: PrintExpr_IsInsideOfApp
    newRightContext = PrintExpr_IsInsideOfApp_Yes

    maybeWrap :: Box -> Box
    maybeWrap = if doWrap exprLeft then wrapInParentheses else identity

    printedLeft :: Box
    printedLeft = printExpr newLeftContext exprLeft

    printedRight :: Box
    printedRight = printExpr newRightContext exprRight

    printed :: Box
    printed = maybeWrap $ printedLeft <<+>> printedRight
  in
    printed
printExpr _ (ExprLambda { binders, body }) = (wrapInParentheses $ punctuateH left (text " ") $ map printBinder binders) <<+>> text "=" <<+>> (printExpr PrintExpr_IsInsideOfApp_No) body
printExpr _ (ExprIf { cond, true_, false_ }) = text "if" <<+>> (printExpr PrintExpr_IsInsideOfApp_No) cond // text "then" <<+>> (printExpr PrintExpr_IsInsideOfApp_No) true_ // text "else" <<+>> (printExpr PrintExpr_IsInsideOfApp_No) false_
printExpr _ (ExprCase { head, branches }) =
  let
    printBranch :: { binders :: NonEmpty Array Binder, body :: Guarded } -> Box
    printBranch { binders, body } = nullBox
  in text "case" <<+>> (punctuateH left (text " ") $ map (printExpr PrintExpr_IsInsideOfApp_No) head) <<+>> text "of" // (vcat left $ map printBranch branches)
printExpr _ (ExprLet { bindings, body }) = nullBox
printExpr _ (ExprDo doStatements) = nullBox
printExpr _ (ExprAdo { statements, result }) = nullBox

printLetBinding :: LetBinding -> Box
printLetBinding (LetBindingSignature { ident, type_ }) = nullBox
printLetBinding (LetBindingName { name, binders, guarded }) = nullBox
printLetBinding (LetBindingPattern { binder, where_ }) = nullBox

printRecordUpdates :: NonEmpty Array RecordUpdate -> Box
printRecordUpdates recordUpdates = text "{" <<+>> (punctuateH left (text ",") $ map printRecordUpdate recordUpdates) <<+>> text "}"

printRecordUpdate :: RecordUpdate -> Box
printRecordUpdate (RecordUpdateLeaf label expr) = textFromNewtype label <<+>> text "=" <<+>> (printExpr PrintExpr_IsInsideOfApp_No) expr
printRecordUpdate (RecordUpdateBranch label recordUpdates) = textFromNewtype label <<+>> text "=" <<+>> printRecordUpdates recordUpdates
