module Language.PS.AST.Printers where

import Language.PS.AST.Types
import Prelude

import Data.Array (cons, fromFoldable, null) as Array
import Data.Char.Unicode (isUpper)
import Data.Either (Either(..), fromRight)
import Data.Foldable (foldMap, intercalate)
import Data.List (List(..))
import Data.List (intercalate) as List
import Data.Map (toUnfoldable) as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Set (member) as Set
import Data.String (joinWith)
import Data.String.CodeUnits (uncons) as SCU
import Data.String.Regex (Regex, regex)
import Data.String.Regex (test) as Regex
import Data.String.Regex.Flags (noFlags) as Regex.Flags
import Data.Tuple (Tuple(..))
import Language.PS.AST.Imports (declarationImports, importsDeclarations)
import Language.PS.AST.Sugar.Type (constructor)
import Matryoshka (Algebra, cata)
import Partial.Unsafe (unsafePartial)

lines :: Array String -> String
lines = joinWith "\n"

line :: Array String -> String
line = joinWith " "

printModule :: Module -> String
printModule (Module { moduleName, declarations }) = lines $
  [ printModuleHead moduleName
  , printedImports imports
  , List.intercalate "\n\n" $ map printDeclaration declarations
  , ""
  ]
  where
    imports = importsDeclarations <<< foldMap declarationImports $ declarations

    printedImports Nil = ""
    printedImports imports = List.intercalate "\n" $ map printImport imports

printImport :: ImportDecl -> String
printImport (ImportDecl { moduleName: ModuleName "Prelude" }) = "import Prelude"
printImport (ImportDecl { moduleName, names }) = line
  [ "import"
  , mn
  , "(" <> (joinWith ", " <<< Array.fromFoldable <<< map printName $ names) <> ")"
  , "as"
  , mn
  ]
  where
    mn = printModuleName moduleName

    printName :: Import -> String
    printName (ImportValue s) = unwrap s
    printName (ImportClass s) = "class " <> unwrap s
    printName (ImportType s) = unwrap s

printModuleHead :: ModuleName -> String
printModuleHead moduleName =
  line ["module", printModuleName moduleName, "where" ]

printDeclaration :: Declaration -> String
printDeclaration (DeclForeignData { typeName: TypeName name }) = -- , "kind": k }) =
  line [ "foreign import data", name, "::", "Type" ] -- fromMaybe "Type" k
printDeclaration (DeclForeignValue { ident: Ident ident, "type": type_ }) = -- , "kind": k }) =
  line [ "foreign import", ident, "::", cata printType type_ StandAlone ] -- fromMaybe "Type" k
printDeclaration (DeclInstance { head, body }) = lines $ Array.cons h (map (indent <<< printValueBindingFields) body)
  where
    indent = append "  "
    h = line
      [ "instance", unwrap head.name, "::", printQualifiedName head.className
      , line (map (flip (cata printType) InApplication) head.types), "where"
      ]
printDeclaration (DeclValue v) = printValueBindingFields v
printDeclaration (DeclType { typeName, "type": type_, vars }) =
  line ["type", unwrap typeName, line $ (map unwrap) vars, "=", cata printType type_ StandAlone]
printDeclaration (DeclData { dataDeclType, typeName, vars, constructors }) =
  (lines [ line [printDeclDataType dataDeclType, unwrap typeName, line $ (map unwrap) vars] ]) <> printedConstructors
  where
    printedConstructors :: String
    printedConstructors =
      constructors
      # map printConstructor
      # joinWith " | "
      # ("= " <> _)

    printConstructor :: { name :: TypeName, types :: Array Type } -> String
    printConstructor constructor = line $ [unwrap constructor.name] <> map (\type_ -> cata printType type_ InApplication) constructor.types

    indent = append "  "

printDeclDataType :: DeclDataType -> String
printDeclDataType DeclDataTypeData = "data"
printDeclDataType DeclDataTypeNewtype = "newtype"

printValueBindingFields :: ValueBindingFields -> String
printValueBindingFields { value: { binders, name: Ident name, expr }, signature } =
  case signature of
    Nothing -> v
    Just s ->
      name <> " :: " <> cata printType s StandAlone
      <> "\n"
      <> v
  where
    bs = if Array.null binders then mempty else " " <> line (map unwrap binders)
    v = name <> bs <> " = " <> (cata printExpr expr { precedence: Zero, binary: Nothing })

printModuleName :: ModuleName -> String
printModuleName (ModuleName n) = n

printQualifiedName :: ∀ n. Newtype n String => QualifiedName n -> String
printQualifiedName { moduleName: Nothing, name } = unwrap name
printQualifiedName { moduleName: Just m, name } = case m of
  (ModuleName "Prelude") -> unwrap name
  otherwise -> printModuleName m <> "." <> unwrap name

-- | I'm not sure about this whole minimal parens printing strategy
-- | so please correct me if I'm wrong.
data Precedence = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
derive instance eqPrecedence :: Eq Precedence
derive instance ordPrecedence :: Ord Precedence
data Branch = BranchLeft | BranchRight
data Associativity = AssocLeft | AssocRight
type ExprPrintingContext =
  { precedence :: Precedence
  , binary :: Maybe
    { assoc :: Associativity
    , branch :: Branch
    }
  }

-- | We are using here GAlgebra to get
-- | a bit nicer output for an application but
-- | probably we should just use the same strategy
-- | as in case of `printType` and pass printing context.
printExpr :: Algebra ExprF (ExprPrintingContext -> String)
printExpr = case _ of
  ExprBoolean b -> const $ show b
  ExprApp x y -> case _ of
    { precedence, binary: Just { assoc, branch }} -> case precedence `compare` Six, assoc, branch of
      GT, _, _ -> "(" <> sub <> ")"
      EQ, AssocLeft, BranchRight -> "(" <> sub <> ")"
      EQ, AssocRight, BranchLeft -> "(" <> sub <> ")"
      _, _, _ -> sub
    { precedence } -> if precedence < Six
      then sub
      else "(" <> sub <> ")"
    where
      sub =
        x { precedence: Six, binary: Just { assoc: AssocLeft, branch: BranchLeft }}
        <> " "
        <> y { precedence: Six, binary: Just { assoc: AssocLeft, branch: BranchRight }}
  ExprArray arr -> const $ "[" <> intercalate ", " (map (_ $ zero ) arr) <> "]"
  ExprIdent x -> const $ printQualifiedName x
  ExprNumber n ->  const $ show n
  ExprRecord props -> const $ "{ " <> intercalate ", " props' <> " }"
    where
      props' :: Array String
      props' = map (\(Tuple n v) -> printRowLabel n <> ": " <> v zero) <<< Map.toUnfoldable $ props

  ExprString s -> const $ show s
  where
    zero = { precedence: Zero, binary: Nothing }

data PrintingContext = StandAlone | InApplication | InArr

printRowLabel :: String -> String
printRowLabel l = case SCU.uncons l of
  Just { head } -> if isUpper head || l `Set.member` reservedNames || not (Regex.test alphanumRegex l)
    then show l
    else l
  Nothing -> l
  where
    alphanumRegex :: Regex
    alphanumRegex = unsafePartial $ fromRight $ regex "^[A-Za-z0-9_]*$" Regex.Flags.noFlags


printType :: Algebra TypeF (PrintingContext -> String)
printType = case _ of
  TypeApp l params -> parens (line $ Array.cons (l InApplication) (map (_ $ InApplication) params))
  TypeArray type_ -> parens $ "Array "  <> type_ StandAlone
  TypeArr f a ->  case _ of
    InArr -> "(" <> s <> ")"
    InApplication -> "(" <> s <> ")"
    otherwise -> s
    where
      s = f InArr <> " -> " <> a StandAlone
  TypeBoolean -> const "Boolean"
  TypeConstructor qn -> const $ printQualifiedName qn
  TypeConstrained { className, params } type_ -> const $
    printQualifiedName className <> " " <> line (map (_ $ InApplication) params) <> " => " <> type_ StandAlone
  TypeForall vs type_ -> const $ "∀" <> " " <> line (map unwrap vs) <> "." <> " " <> type_ StandAlone
  TypeNumber -> const "Number"
  TypeRecord r -> const $ "{ " <> printRow r <> " }"
  TypeRow r -> const $ "( " <> printRow r <> " )"
  TypeString -> const $ "String"
  TypeVar (Ident v) -> const $ v
  where
    parens s InArr = s
    parens s InApplication = "(" <> s <> ")"
    parens s StandAlone = s

    printRow (Row { labels, tail }) = intercalate ", " labels' <> tail'
      where
        labels' :: Array String
        labels' = map (\(Tuple n type_) -> printRowLabel n <> " :: " <> type_ StandAlone) <<< Map.toUnfoldable $ labels

        tail' = case tail of
          Nothing -> mempty
          Just (Left ident) -> " | " <> unwrap ident
          Just (Right qn) -> " | " <> printQualifiedName qn
