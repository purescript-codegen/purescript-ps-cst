module Language.PS.AST.Printers.TypeLevel where

import Language.PS.AST.Printers.PrintImports
import Language.PS.AST.Printers.PrintModuleModuleNameAndExports
import Language.PS.AST.Printers.Utils
import Language.PS.AST.Types
import Prelude

import Data.Array (snoc) as Array
import Data.Either (Either(..))
import Data.Foldable (null)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Variant (contract)
import Text.PrettyPrint.Boxes (Box, left, nullBox, punctuateH, text, vcat, vsep, (//), (<<+>>), (<<>>))

printFundep :: ClassFundep -> Box
printFundep (FundepDetermines lefts rights) = (punctuateH left emptyColumn $ map textFromNewtype lefts) <<+>> text "->" <<+>> (punctuateH left emptyColumn $ map textFromNewtype rights)

printFixity :: Fixity -> Box
printFixity Infix  = text "infix"
printFixity Infixl = text "infixl"
printFixity Infixr = text "infixr"

printDataCtor :: DataCtor -> Box
printDataCtor (DataCtor dataCtor) =
  let
    doWrap :: Type -> Boolean
    doWrap (TypeApp _ _) = true
    doWrap (TypeForall _ _) = true
    doWrap (TypeArr _ _) = true
    doWrap (TypeOp _ _ _) = true
    doWrap (TypeConstrained _ _) = true
    doWrap _ = false

    maybeWrap x = if doWrap x then wrapInParentheses else identity

    context = { printType_Style: PrintType_Multiline, printType_IsInsideOfApp: PrintType_IsInsideOfApp_No }

    printType' :: Type -> Box
    printType' type_ = maybeWrap type_ $ printType context $ type_

    name = textFromNewtype dataCtor.dataCtorName

    fields = dataCtor.dataCtorFields <#> printType'

    printedFields = vcat left fields
  in
    name <<+>> printedFields

printDataHead :: Box -> DataHead -> Box
printDataHead reservedWord (DataHead dataHead) =
  let
    head = reservedWord <<+>> textFromNewtype dataHead.dataHdName

    vars = map printTypeVarBinding dataHead.dataHdVars
  in
    if null vars then head else head <<+>> punctuateH left (emptyColumn) vars

printTypeVarBinding :: TypeVarBinding -> Box
printTypeVarBinding (TypeVarName ident) = textFromNewtype ident
printTypeVarBinding (TypeVarKinded ident kind_) = wrapInParentheses $ textFromNewtype ident <<+>> text "::" <<+>> printKind kind_

printKind :: Kind -> Box
printKind (KindName qualifiedKindName) = printQualifiedName_AnyProperNameType qualifiedKindName
printKind (KindArr kindLeft_ kindRight_) =
  let
    isComplex :: Kind -> Boolean
    isComplex (KindArr _ _) = true
    isComplex _ = false

    printedLeft = printKind kindLeft_

    printedLeft' = if isComplex kindLeft_ then wrapInParentheses printedLeft else printedLeft
  in
    printedLeft' <<+>> text "->" <<+>> printKind kindRight_
printKind (KindRow kind_) = text "#" <<+>> printKind kind_
printKind (KindParens kind_) = wrapInParentheses $ printKind kind_

printQualifiedName_Ident :: ∀ proxy. QualifiedName Ident -> Box
printQualifiedName_Ident (QualifiedName qualifiedName) = case qualifiedName.qualModule of
  Nothing -> textFromNewtype qualifiedName.qualName
  (Just moduleName) -> printModuleName moduleName <<>> text "." <<>> textFromNewtype qualifiedName.qualName

printQualifiedName_AnyProperNameType :: ∀ proxy. QualifiedName (ProperName proxy) -> Box
printQualifiedName_AnyProperNameType (QualifiedName qualifiedName) = case qualifiedName.qualModule of
  Nothing -> textFromNewtype qualifiedName.qualName
  (Just moduleName) -> printModuleName moduleName <<>> text "." <<>> textFromNewtype qualifiedName.qualName

printQualifiedName_AnyOpNameType :: ∀ proxy. QualifiedName (OpName proxy) -> Box
printQualifiedName_AnyOpNameType (QualifiedName qualifiedName) = case qualifiedName.qualModule of
  Nothing -> textFromNewtype qualifiedName.qualName
  (Just moduleName) -> printModuleName moduleName <<>> text "." <<>> wrapInParentheses (textFromNewtype qualifiedName.qualName)

-- Prefer multiline when first enter the rendering function, prefer one line when inside of row extensions (i.e. `MyExt + MyOtherExt` in `( foo :: Bar | MyExt + MyOtherExt )`)
data PrintType_Style
  = PrintType_Multiline
  | PrintType_OneLine

-- Am I inside of TypeApp that didn't yet break? (i.e. TypeApp inside TypeApp)
-- used to prevent multiple wraps
-- e.g. prevents `(((Complex A) B) C)`
-- expected `Complex A B C`
data PrintType_IsInsideOfApp
  = PrintType_IsInsideOfApp_Yes
  | PrintType_IsInsideOfApp_No

type PrintType_Context
  = { printType_Style :: PrintType_Style, printType_IsInsideOfApp :: PrintType_IsInsideOfApp }

initialPrintType_Context :: PrintType_Style -> PrintType_Context
initialPrintType_Context printType_Style = { printType_Style: printType_Style, printType_IsInsideOfApp: PrintType_IsInsideOfApp_No }

resetPrintType_IsInsideOfApp :: PrintType_Context -> PrintType_Context
resetPrintType_IsInsideOfApp printType_Context = printType_Context { printType_IsInsideOfApp = PrintType_IsInsideOfApp_No }

printType :: PrintType_Context -> Type -> Box
printType printType_Context (TypeVar ident) = textFromNewtype ident
printType printType_Context (TypeConstructor qualifiedTypeName) = printQualifiedName_AnyProperNameType qualifiedTypeName
printType printType_Context TypeWildcard = text "_"
printType printType_Context (TypeHole ident) = text "?" <<>> textFromNewtype ident
printType printType_Context (TypeString string) = text "\"" <<>> text string <<>> text "\""
printType printType_Context (TypeRow row) = printRowLikeType (resetPrintType_IsInsideOfApp printType_Context) (text "(") (text ")") row
printType printType_Context (TypeRecord row) = printRowLikeType (resetPrintType_IsInsideOfApp printType_Context) (text "{") (text "}") row
printType printType_Context (TypeApp leftType rightType) =
  let
    doWrap :: Type -> Boolean
    doWrap (TypeApp _ _) =
      case printType_Context.printType_IsInsideOfApp of
        PrintType_IsInsideOfApp_No -> false
        PrintType_IsInsideOfApp_Yes -> true
    doWrap (TypeForall _ _) = true
    doWrap (TypeArr _ _) = true
    doWrap (TypeOp _ _ _) = true
    doWrap (TypeConstrained _ _) = true
    doWrap _ = false

    newLeftContext :: PrintType_Context
    newLeftContext = { printType_Style: PrintType_OneLine, printType_IsInsideOfApp: PrintType_IsInsideOfApp_No }

    newRightContext :: PrintType_Context
    newRightContext = { printType_Style: PrintType_OneLine, printType_IsInsideOfApp: PrintType_IsInsideOfApp_Yes }

    maybeWrap :: Box -> Box
    maybeWrap = if doWrap leftType then wrapInParentheses else identity

    printedLeft :: Box
    printedLeft = printType newLeftContext leftType

    printedRight :: Box
    printedRight = printType newRightContext rightType

    printed :: Box
    printed = maybeWrap $ printedLeft <<+>> printedRight
  in
    printed
printType printType_Context (TypeForall typeVarBindings type_) =
  let
    newContext = resetPrintType_IsInsideOfApp printType_Context
  in
    text "forall" <<+>> punctuateH left (emptyColumn) (map printTypeVarBinding typeVarBindings) <<+>> text "." <<+>> printType newContext type_
printType printType_Context (TypeArr leftType rightType) =
  let
    newContext = resetPrintType_IsInsideOfApp printType_Context
  in
    printType newContext leftType <<+>> text "->" <<+>> printType newContext rightType
printType printType_Context (TypeKinded type_ kind_) =
  let
    newContext = resetPrintType_IsInsideOfApp printType_Context
  in
    wrapInParentheses $ printType newContext type_ <<+>> text "::" <<+>> printKind kind_
printType printType_Context (TypeOp leftType qualifiedOpName rightType) =
  let
    newContext = resetPrintType_IsInsideOfApp printType_Context
  in
    printType newContext leftType <<+>> printQualifiedName_AnyOpNameType qualifiedOpName <<+>> printType newContext rightType
printType printType_Context (TypeConstrained constraint type_) =
  let
    newContext = { printType_Style: PrintType_OneLine, printType_IsInsideOfApp: PrintType_IsInsideOfApp_No }
  in
    printConstraint constraint <<+>> text "=>" <<+>> printType newContext type_

printConstraint :: Constraint -> Box
printConstraint (Constraint { className, args }) =
  let
    context = { printType_Style: PrintType_OneLine, printType_IsInsideOfApp: PrintType_IsInsideOfApp_No }
  in
    if null args
      then printQualifiedName_AnyProperNameType className
      else printQualifiedName_AnyProperNameType className <<+>> (punctuateH left (emptyColumn) $ map (printType context) args)
printConstraint (ConstraintParens constraint) = wrapInParentheses $ printConstraint constraint

printRowLikeType :: PrintType_Context -> Box -> Box -> Row -> Box
printRowLikeType _ leftWrapper rightWrapper row@(Row { rowLabels: [], rowTail: Nothing }) = leftWrapper <<>> rightWrapper
printRowLikeType _ leftWrapper rightWrapper row@(Row { rowLabels: [], rowTail: Just rowTail }) =
  let
    context = { printType_Style: PrintType_OneLine, printType_IsInsideOfApp: PrintType_IsInsideOfApp_No }
  in
    leftWrapper <<+>> text "|" <<+>> printType context rowTail <<+>> rightWrapper
printRowLikeType ({ printType_Style: PrintType_OneLine }) leftWrapper rightWrapper row@(Row { rowLabels, rowTail }) =
  let
    context = { printType_Style: PrintType_OneLine, printType_IsInsideOfApp: PrintType_IsInsideOfApp_No }

    printedTail = rowTail <#> printType context <#> (text "|" <<+>> _)

    printedRowLabels =
      rowLabels
        <#> printRowLabel context
        # punctuateH left (text ", ")
        # maybe identity (\tail rowLine -> rowLine <<+>> tail) printedTail
        # (\x -> leftWrapper <<+>> x <<+>> rightWrapper)
  in
    printedRowLabels
printRowLikeType ({ printType_Style: PrintType_Multiline }) leftWrapper rightWrapper row@(Row { rowLabels, rowTail }) =
  let
    context = { printType_Style: PrintType_Multiline, printType_IsInsideOfApp: PrintType_IsInsideOfApp_No }

    printedTail = rowTail <#> printType context <#> (text "|" <<+>> _)

    printedRowLabels =
      rowLabels
        <#> printRowLabel context
        # mapWithIndex (\i box -> ifelse (i == 0) leftWrapper (text ",") <<+>> box)
        # maybe identity (flip Array.snoc) printedTail
        # flip Array.snoc rightWrapper
        # vcat left
  in
    printedRowLabels

printRowLabel :: PrintType_Context -> { label :: Label, type_ :: Type } -> Box
printRowLabel printType_Context { label, type_ } = textFromNewtype label <<+>> text "::" <<+>> printType printType_Context type_
