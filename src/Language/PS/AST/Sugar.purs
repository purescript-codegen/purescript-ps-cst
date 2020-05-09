module Language.PS.AST.Sugar where

import Language.PS.AST.Printers
import Language.PS.AST.Types
import Prelude

import Data.Foldable (foldMap)
import Data.Functor.Mu (roll)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.NonEmpty (NonEmpty(..))
import Data.String.Extra (camelCase)
import Data.Tuple.Nested (type (/\), (/\))

mkModuleName :: NonEmpty Array String -> ModuleName
mkModuleName = ModuleName <<< map wrap

nonQualifiedName :: ∀ a . a -> QualifiedName a
nonQualifiedName a = QualifiedName { qualModule: Nothing, qualName: a }

qualifiedName :: ∀ a . ModuleName -> a -> QualifiedName a
qualifiedName moduleName a = QualifiedName { qualModule: Just moduleName, qualName: a }

typeRecord :: Array (String /\ Type) -> Type
typeRecord labels =
  TypeRecord $ Row
    { rowLabels: mkRowLabels labels
    , rowTail: Nothing
    }

typeRow :: Array (String /\ Type) -> Type
typeRow labels =
  TypeRow $ Row
    { rowLabels: mkRowLabels labels
    , rowTail: Nothing
    }

mkRowLabels :: Array (String /\ Type) -> Array { label :: Label, type_ :: Type }
mkRowLabels = map mkRowLabel

mkRowLabel :: (String /\ Type) -> { label :: Label, type_ :: Type }
mkRowLabel = (\(label /\ type_) -> { label: Label label, type_ })

nonQualifiedNameTypeConstructor :: String → Type
nonQualifiedNameTypeConstructor s = TypeConstructor $ nonQualifiedName (ProperName s)

booleanType :: Type
booleanType = nonQualifiedNameTypeConstructor "Boolean"

numberType :: Type
numberType = nonQualifiedNameTypeConstructor "Number"

stringType :: Type
stringType = nonQualifiedNameTypeConstructor "String"

arrayType :: Type -> Type
arrayType = TypeApp (nonQualifiedNameTypeConstructor "Array")

maybeType :: Type -> Type
maybeType = TypeApp (nonQualifiedNameTypeConstructor "Maybe")

typeVarName :: String → TypeVarBinding
typeVarName = TypeVarName <<< Ident -- for left side of forall

typeVar :: String → Type
typeVar = TypeVar <<< Ident -- for right side of forall

kindNamed :: String → Kind
kindNamed s = KindName (nonQualifiedName $ ProperName s)

nonQualifiedExprIdent :: String → Expr
nonQualifiedExprIdent s = ExprIdent $ nonQualifiedName (Ident s)

-- emptyRow :: Row
-- emptyRow = Row { labels: mempty, tail: Nothing }

-- declType :: TypeName -> Array Ident -> Type -> { declaration :: Declaration , constructor :: Type }
-- declType typeName vars body =
--   let
--     declaration = DeclType
--       { typeName, "type": body, vars }
--     constructor = roll $ TypeConstructor { name: typeName, moduleName: Nothing }
--   in
--     { declaration, constructor }

-- declForeignData :: TypeName -> { declaration :: Declaration, constructor :: Type }
-- declForeignData typeName =
--   let
--     declaration = DeclForeignData { typeName }
--     constructor = roll $ TypeConstructor { name: typeName, moduleName: Nothing }
--   in
--     { declaration, constructor }

-- valueBindingFields :: Ident -> Array Ident -> Expr -> Maybe Type -> ValueBindingFields
-- valueBindingFields name binders expr signature = { value: { binders, expr, name }, signature }

-- declValue :: Ident -> Array Ident -> Expr -> Maybe Type -> { declaration :: Declaration, var :: Expr }
-- declValue name binders expr signature =
--   let
--     declaration = DeclValue (valueBindingFields name binders expr signature)
--     var = roll $ ExprIdent { name, moduleName: Nothing }
--   in
--     { declaration, var }

-- declForeignValue :: Ident -> Type -> { declaration :: Declaration, var :: Expr }
-- declForeignValue ident t =
--   let
--     declaration = DeclForeignValue { ident, "type": t }
--     var = roll $ ExprIdent { name: ident, moduleName: Nothing }
--   in
--     { declaration, var }

-- TODO: use type hash?
-- declInstance :: QualifiedName ClassName -> Array Type -> Array ValueBindingFields -> Declaration
-- declInstance className types body = DeclInstance
--   { head:
--     { className
--     , name: Ident $ camelCase $
--         strinifyQualifiedName className <> foldMap (flip (cata printType) StandAlone) types
--     , types
--     }
--   , body
--   }

