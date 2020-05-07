module Language.PS.AST.Printers where

import Prelude
import Language.PS.AST.Types
import Language.PS.AST.Printers.Utils
import Language.PS.AST.Printers.PrintImports
import Language.PS.AST.Printers.PrintModuleModuleNameAndExports
import Text.PrettyPrint.Boxes

import Data.Array (cons, fromFoldable, null) as Array
import Data.Char.Unicode (isUpper)
import Data.Either (Either(..), fromRight)
import Data.Foldable (class Foldable, foldMap, intercalate, length, null)
import Data.List (List(..))
import Data.List (fromFoldable, intercalate) as List
import Data.Map (toUnfoldable) as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Set (member) as Set
import Data.String (joinWith)
import Data.String.CodeUnits (uncons) as SCU
import Data.String.Regex (Regex, regex)
import Data.String.Regex (test) as Regex
import Data.String.Regex.Flags (noFlags) as Regex.Flags
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicate)
import Matryoshka (Algebra, cata)
import Partial.Unsafe (unsafePartial)

printModule :: Module -> Box
printModule (Module { moduleName, imports, exports, declarations }) = lines $
  [ printModuleModuleNameAndExports moduleName exports
  , printImports imports
  , printDeclarations declarations
  , emptyRow
  ]

printDeclarations :: Array Declaration -> Box
printDeclarations [] = nullBox
printDeclarations declarations =
  emptyRow
  // (vsep 1 left $ map printDeclaration declarations)

printDeclaration :: Declaration -> Box
printDeclaration (DeclData dataHead []) = printDataHead dataHead
printDeclaration (DeclData dataHead arrayDataCtor) =
  let
    head = printDataHead dataHead

    printCtors :: DataCtor -> Box
    printCtors (DataCtor dataCtor) = textFromNewtype dataCtor.dataCtorName

    printedNamesColumn = vcat left $ map printCtors arrayDataCtor
    separatorsColumn = vcat left $ [text "="] <> replicate (length arrayDataCtor - 1) (text "|")
    printedCtors = twoSpaceIdentation <<>> separatorsColumn <<+>> printedNamesColumn
  in head // printedCtors
printDeclaration (DeclType dataHead type_) = nullBox

printDataHead :: DataHead -> Box
printDataHead (DataHead dataHead) =
  let
    head = text "data" <<+>> textFromNewtype dataHead.dataHdName
    vars = map printTypeVarBinding dataHead.dataHdVars
   in if null vars then head else head <<+>> punctuateH left (text " ") vars

printTypeVarBinding :: TypeVarBinding -> Box
printTypeVarBinding (TypeVarName ident) = textFromNewtype ident
printTypeVarBinding (TypeVarKinded ident kind_) = wrapInParentheses $ textFromNewtype ident <<+>> text " :: " <<+>> printKind kind_

printKind :: Kind -> Box
printKind (KindName (QualifiedName qualifiedKindName)) =
  case qualifiedKindName.qualModule of
       Nothing -> textFromNewtype qualifiedKindName.qualName
       (Just moduleName) -> printModuleName moduleName <<>> text "." <<>> textFromNewtype qualifiedKindName.qualName
printKind (KindArr kindLeft_ kindRight_) = printKind kindLeft_ <<+>> text "->" <<+>> printKind kindRight_
printKind (KindRow kind_) = text "#" <<+>> printKind kind_
printKind (KindParens kind_) = wrapInParentheses $ printKind kind_

printType :: Type -> Box
printType (TypeVar ident) = textFromNewtype ident

--   line
--     [ text "foreign import data"
--     , text $ name
--     , text "::"
--     , text "Type"
--     ] -- fromMaybe "Type" k
-- printDeclaration (DeclForeignValue { ident: Ident ident, "type": type_ }) = -- , "kind": k }) =
--   line
--     [ text "foreign import"
--     , text ident
--     , text "::"
--     , cata printType type_ StandAlone
--     ] -- fromMaybe "Type" k
-- printDeclaration (DeclInstance { head, body }) = lines $ Array.cons h (map (indent <<< printValueBindingFields) body)
--   where
--     indent = appendHorizTop (emptyBox 1 2)

--     h = line
--       [ text "instance"
--       , text $ unwrap head.name
--       , text "::"
--       , printQualifiedName head.className
--       , line (map (flip (cata printType) InApplication) head.types)
--       , text "where"
--       ]
-- printDeclaration (DeclValue v) = printValueBindingFields v
-- printDeclaration (DeclType { typeName, "type": type_, vars }) =
--   line
--     [ text "type"
--     , text $ unwrap typeName
--     , line $ (map (text <<< unwrap)) vars
--     , text "="
--     , cata printType type_ StandAlone
--     ]
-- printDeclaration (DeclData { dataDeclType, typeName, vars, constructors }) =
--   (lines
--     [ line
--       [ printDeclDataType dataDeclType
--       , text $ unwrap typeName
--       , line $ (map (text <<< unwrap)) vars
--       ]
--     ]
--   ) <<>> printedConstructors
--   where
--     printedConstructors :: Box
--     printedConstructors =
--       constructors
--       # map printConstructor
--       # punctuateH left (text " | ")
--       # (text "= " <<>> _)

--     printConstructor :: { name :: TypeName, types :: Array Type } -> Box
--     printConstructor constructor = line $ [text $ unwrap constructor.name] <> map (\type_ -> cata printType type_ InApplication) constructor.types

--     indent = appendHorizTop (emptyBox 1 2)

-- printDeclDataType :: DeclDataType -> Box
-- printDeclDataType DeclDataTypeData = text "data"
-- printDeclDataType DeclDataTypeNewtype = text "newtype"

-- printValueBindingFields :: ValueBindingFields -> Box
-- printValueBindingFields { value: { binders, name: Ident name, expr }, signature } =
--   case signature of
--     Nothing -> v
--     Just s ->
--       text name
--       <<>> text " :: "
--       <<>> cata printType s StandAlone
--       /+/
--       v
--   where
--     bs = if Array.null binders then nullBox else text " " <<>> line (map (text <<< unwrap) binders)

--     v = text name <<>> bs <<>> text " = " <<>> (cata printExpr expr { precedence: Zero, binary: Nothing })

-- printQualifiedName :: ∀ n. Newtype n String => QualifiedName n -> Box
-- printQualifiedName = text <<< strinifyQualifiedName

-- strinifyQualifiedName :: ∀ n. Newtype n String => QualifiedName n -> String
-- strinifyQualifiedName { moduleName: Nothing, name } = unwrap name
-- strinifyQualifiedName { moduleName: Just m, name } = case m of
--   (ModuleName "Prelude") -> unwrap name
--   otherwise -> strintifyModuleName m <> "." <> unwrap name

-- -- | I'm not sure about this whole minimal parens printing strategy
-- -- | so please correct me if I'm wrong.
-- data Precedence = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
-- derive instance eqPrecedence :: Eq Precedence
-- derive instance ordPrecedence :: Ord Precedence
-- data Branch = BranchLeft | BranchRight
-- data Associativity = AssocLeft | AssocRight
-- type ExprPrintingContext =
--   { precedence :: Precedence
--   , binary :: Maybe
--     { assoc :: Associativity
--     , branch :: Branch
--     }
--   }

-- -- | We are using here GAlgebra to get
-- -- | a bit nicer output for an application but
-- -- | probably we should just use the same strategy
-- -- | as in case of `printType` and pass printing context.
-- printExpr :: Algebra ExprF (ExprPrintingContext -> Box)
-- printExpr = case _ of
--   ExprBoolean b -> const $ text $ show b
--   ExprApp x y -> case _ of
--     { precedence, binary: Just { assoc, branch }} -> case precedence `compare` Six, assoc, branch of
--       GT, _, _ -> text "(" <<>> sub <<>> text ")"
--       EQ, AssocLeft, BranchRight -> text "(" <<>> sub <<>> text ")"
--       EQ, AssocRight, BranchLeft -> text "(" <<>> sub <<>> text ")"
--       _, _, _ -> sub
--     { precedence } -> if precedence < Six
--       then sub
--       else text "(" <<>> sub <<>> text ")"
--     where
--       sub =
--         x { precedence: Six, binary: Just { assoc: AssocLeft, branch: BranchLeft }}
--         <<>> text " "
--         <<>> y { precedence: Six, binary: Just { assoc: AssocLeft, branch: BranchRight }}
--   ExprArray arr -> const $ text "[" <<>> punctuateH left (text ", ") (map (_ $ zero ) arr) <<>> text "]"
--   ExprIdent x -> const $ printQualifiedName x
--   ExprNumber n ->  const $ text $ show n
--   ExprRecord props -> const $ text "{ " <<>> punctuateH left (text ", ") props' <<>> text " }"
--     where
--       props' :: Array Box
--       props' = map (\(Tuple n v) -> printRowLabel n <<>> text ": " <<>> v zero) <<< Map.toUnfoldable $ props

--   ExprString s -> const $ text $ show s
--   where
--     zero = { precedence: Zero, binary: Nothing }

-- data PrintingContext = StandAlone | InApplication | InArr

-- printRowLabel :: String -> Box
-- printRowLabel l = case SCU.uncons l of
--   Just { head } -> if isUpper head || l `Set.member` reservedNames || not (Regex.test alphanumRegex l)
--     then text $ show l
--     else text $ l
--   Nothing -> text $ l
--   where
--     alphanumRegex :: Regex
--     alphanumRegex = unsafePartial $ fromRight $ regex "^[A-Za-z0-9_]*$" Regex.Flags.noFlags


-- printType :: Algebra TypeF (PrintingContext -> Box)
-- printType = case _ of
--   TypeApp l params -> parens (line $ Array.cons (l InApplication) (map (_ $ InApplication) params))
--   TypeArray type_ -> parens $ text "Array "  <<>> type_ StandAlone
--   TypeArr f a ->  case _ of
--     InArr -> text "(" <<>> s <<>> text ")"
--     InApplication -> text "(" <<>> s <<>> text ")"
--     otherwise -> s
--     where
--       s = f InArr <<>> text " -> " <<>> a StandAlone
--   TypeBoolean -> const $ text "Boolean"
--   TypeConstructor qn -> const $ printQualifiedName qn
--   TypeConstrained { className, params } type_ -> const $
--     printQualifiedName className <<>> text " " <<>> line (map (_ $ InApplication) params) <<>> text " => " <<>> type_ StandAlone
--   TypeForall vs type_ -> const $ text "∀" <<>> text " " <<>> line (map (text <<< unwrap) vs) <<>> text "." <<>> text " " <<>> type_ StandAlone
--   TypeNumber -> const $ text "Number"
--   TypeRecord r -> const $ text "{ " <<>> printRow r <<>> text " }"
--   TypeRow r -> const $ text "( " <<>> printRow r <<>> text " )"
--   TypeString -> const $ text "String"
--   TypeVar (Ident v) -> const $ text $ v
--   where
--     parens s InArr = s
--     parens s InApplication = text "(" <<>> s <<>> text ")"
--     parens s StandAlone = s

--     printRow (Row { labels, tail }) = punctuateH left (text ", ") labels' <<>> tail'
--       where
--         labels' :: Array Box
--         labels' = map (\(Tuple n type_) -> printRowLabel n <<>> text " :: " <<>> type_ StandAlone) <<< Map.toUnfoldable $ labels

--         tail' = case tail of
--           Nothing -> nullBox
--           Just (Left ident) -> text " | " <<>> (text $ unwrap ident)
--           Just (Right qn) -> text " | " <<>> printQualifiedName qn
