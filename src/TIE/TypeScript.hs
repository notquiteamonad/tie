{-|
Module: TIE.TypeScript

TypeScript type constructs for writing to a TypeScript definition file.
-}
module TIE.TypeScript
  ( -- * Top-Level Access for Writing Documents
    Document (..)
  , writeDocument
    -- * Representations of TypeScript Types
  , Namespace (..)
  , Exported (..)
  , NamespaceMembers
  , NamespaceMember (..)
  , Interface (..)
  , Function (..)
  , Members
  , Member (..)
  , Arguments
  , Argument (..)
  , ReturnType
  -- * Atom-Level TypeScript Types
  , TSType (..)
  -- ** Name Wrappers
  , PrimitiveName (..)
  , NamespaceName (..)
  , InterfaceName (..)
  , ReferenceName (..)
  , AliasName (..)
  , PropertyName (..)
  , FunctionName (..)
  , ArgumentName (..)
  ) where

import           Data.Text (replace)

-- |A top-level construct which contains a complete TypeScript definition file.
newtype Document = Document [Namespace] deriving (Eq, Ord, Show)

-- |Converts a `Document` into `Text` to be written to a "*.d.ts" file.
writeDocument :: Document -> Text
writeDocument (Document xs) =
  "// This file was generated automatically by TIE (TypeScript Interoperator for Elm).\n\n"
  <> replace "; }" " }" (onePerLine (Indented 0) (\i n -> "export " <> writeNamespace i n) xs)

data Indentation
  = Indented Int
  | Inline

onePerLine :: Indentation -> (Indentation -> a -> Text) -> [a] -> Text
onePerLine _ _ [] = ""
onePerLine i@(Indented indentation) write xs = renderedIndentation <> (mconcat . intersperse ("\n" <> renderedIndentation) $ fmap (write i) xs) <> "\n"
  where renderedIndentation = toText (replicate indentation ' ')
onePerLine Inline _ _ = error "Internal: onePerLine cannot be called inline"

nextIndentation :: Indentation -> Indentation
nextIndentation (Indented indentation) = Indented $ indentation + 2
nextIndentation Inline                 = Inline

-- |A TypeScript <https://www.typescriptlang.org/docs/handbook/namespaces.html Namespace>
data Namespace = Namespace NamespaceName NamespaceMembers
  deriving (Eq, Ord, Show)

-- |Determined whether the member should be prefixed with the
-- <https://www.typescriptlang.org/docs/handbook/modules.html#export export> keyword
data Exported = Exported | Private
  deriving (Eq, Ord, Show)

-- |A list of `NamespaceMember`s
type NamespaceMembers = [NamespaceMember]

-- |A valid child of a `Namespace`
data NamespaceMember
  = NMNamespace Exported Namespace -- ^An inner `Namespace`
  | NMAlias AliasName TSType -- ^A <https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#type-aliases type alias>
  | NMInterface Interface -- ^An `Interface` contained within this `Namespace`
  | NMFunction Exported Function -- ^A `Namespace`-level `Function`
  deriving (Eq, Ord, Show)

-- |A TypeScript <https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#interfaces interface>
data Interface = Interface Exported InterfaceName Members
  deriving (Eq, Ord, Show)

-- |A named <https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#functions function> type
data Function = Function FunctionName Arguments ReturnType
  deriving (Eq, Ord, Show)

-- |A list of `Member`s
type Members = [Member]

-- |A valid child of an `Interface` or `MPropertyGroup`
data Member
  = MPropertyGroup PropertyName Members -- ^A named group of `Members`
  | MProperty PropertyName TSType -- ^A property of a particular `TSType`
  | MFunction Function -- ^A contained `Function`
  deriving (Eq, Ord, Show)

-- |A list of `Argument`s
type Arguments = [Argument]

-- |An <https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#functions argument>
-- (or parameter) to a `Function`
data Argument = Argument ArgumentName TSType
  deriving (Eq, Ord, Show)

-- |The <https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#functions return type> of
-- a `Function`.
type ReturnType = TSType

-- |An atom-level TypeScript type
data TSType
  = TReference ReferenceName -- ^A reference to another type such as an `NMAlias` or `Interface`
  | TInlineInterface Members -- ^A type used for defining interface types without naming them
  | TFunction Arguments ReturnType -- ^A function type which specifies its `Arguments` and `ReturnType`
  | TPrimitive PrimitiveName -- ^A primitive type represented by `PrimitiveName`
  | TArray TSType -- ^An <https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#arrays array type>
  | TUnion TSType TSType -- ^A <https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#union-types union between two types>
  | TLiteral Text -- ^A literal text representation of a TypeScript type
  deriving (Ord, Show)

instance Eq TSType where
  TReference a == TReference b = a == b
  TInlineInterface a == TInlineInterface b = a == b
  TFunction a b == TFunction c d = a == c && b == d
  TPrimitive a == TPrimitive b = a == b
  TArray a == TArray b = a == b
  TUnion a b == TUnion c d = (a == c && b == d) || (a == d && b == c)
  TLiteral a == TLiteral b = a == b
  _ == _ = False

instance Semigroup TSType where
  lhs@(TPrimitive a) <> rhs@(TPrimitive b)
    | a == b = lhs
    | otherwise = lhs `TUnion` rhs
  lhs@(TUnion a b) <> rhs@(TUnion c d)
    | (a == c && b == d) || (a == d && b == c) = lhs
    | otherwise = lhs `TUnion` rhs
  lhs@(a@(TPrimitive _) `TUnion` b@(TPrimitive _)) <> rhs
    | a == rhs || b == rhs = lhs
    | otherwise = lhs `TUnion` rhs
  lhs <> rhs@(a@(TPrimitive _) `TUnion` b@(TPrimitive _))
    | a == lhs || b == lhs = rhs
    | otherwise = lhs `TUnion` rhs
  a <> b = a `TUnion` b

{-|
  A representation of
  <https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#the-primitives-string-number-and-boolean TypeScript primitive types>
  along with the special types of <https://www.typescriptlang.org/docs/handbook/2/functions.html#unknown unknown>,
  <https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#null-and-undefined null>, and
  <https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#null-and-undefined void>.
-}
data PrimitiveName
  = PString -- ^ Represents TypeScript's "string"
  | PNumber -- ^ Represents TypeScript's "number"
  | PBoolean -- ^ Represents TypeScript's "boolean"
  | PUnknown -- ^ Represents TypeScript's "unknown"
  | PNull -- ^ Represents TypeScript's "null"
  | PVoid -- ^ Represents TypeScript's "void"
  deriving (Eq, Ord, Show)

-- |A wrapper for the name of a `Namespace`
newtype NamespaceName = NamespaceName Text deriving (Eq, Ord, Show)

-- |A wrapper for the name of an `Interface`
newtype InterfaceName = InterfaceName Text deriving (Eq, Ord, Show)

-- |A wrapper for the name of a reference to an `NMAlias` or `Interface`
newtype ReferenceName = ReferenceName Text deriving (Eq, Ord, Show)

-- |A wrapper for the name of an `NMAlias`
newtype AliasName = AliasName Text deriving (Eq, Ord, Show)

-- |A wrapper for the name of an `MProperty` or `MPropertyGroup`
newtype PropertyName = PropertyName Text deriving (Eq, Ord, Show)

-- |A wrapper for the name of a `Function`
newtype FunctionName = FunctionName Text deriving (Eq, Ord, Show)

-- |A wrapper for the name of an `Argument` of a `Function`
newtype ArgumentName = ArgumentName Text deriving (Eq, Ord, Show)

writeNamespace :: Indentation -> Namespace -> Text
writeNamespace indentation (Namespace (NamespaceName name) members) =
  "namespace "
  <> name
  <> writeOpeningBrace indentation
  <> onePerLine (nextIndentation indentation) writeNamespaceMember members
  <> writeClosingBrace indentation

writeExported :: Exported -> Text
writeExported Exported = "export "
writeExported Private  = ""

writeOpeningBrace :: Indentation -> Text
writeOpeningBrace (Indented _) = " {\n"
writeOpeningBrace Inline       = "{ "

writeClosingBrace :: Indentation -> Text
writeClosingBrace (Indented indentation) =  toText (replicate indentation ' ') <> "}"
writeClosingBrace Inline =  " }"

writeNamespaceMember :: Indentation -> NamespaceMember -> Text
writeNamespaceMember indentation (NMNamespace exported n) = writeExported exported <> writeNamespace indentation n
writeNamespaceMember _ (NMAlias name t) = writeAlias name t
writeNamespaceMember indentation (NMInterface i) = writeInterface indentation i
writeNamespaceMember _ (NMFunction exported f)  = writeExported exported <> "function " <> writeFunction f

writeAlias :: AliasName -> TSType -> Text
writeAlias (AliasName name) aliasFor =
  writeExported Exported
  <> "type "
  <> name
  <> " = "
  <> writeTSType aliasFor
  <> ";"

writeInterface :: Indentation -> Interface -> Text
writeInterface indentation (Interface exported (InterfaceName name) members) =
  writeExported exported
  <> "interface "
  <> name
  <> writeOpeningBrace indentation
  <> onePerLine (nextIndentation indentation) writeMember members
  <> writeClosingBrace indentation

writeMember :: Indentation -> Member -> Text
writeMember indentation (MPropertyGroup name members) = writePropertyGroup indentation name members
writeMember _ (MProperty name t)       = writeProperty name t
writeMember _ (MFunction f)       = writeFunction f

writePropertyGroup :: Indentation -> PropertyName -> Members -> Text
writePropertyGroup indentation (PropertyName name) members =
  name
  <> ":"
  <> writeOpeningBrace indentation
  <> onePerLine (nextIndentation indentation) writeMember members
  <> writeClosingBrace indentation
  <> ";"

writeProperty :: PropertyName -> TSType -> Text
writeProperty (PropertyName name) t = name <> ": " <> writeTSType t <> ";"

writeTSType :: TSType -> Text
writeTSType (TReference (ReferenceName i)) = i
writeTSType (TInlineInterface members) = writeInlineInterface members
writeTSType (TFunction args returnType)                 = writeAnonymousFunctionType args returnType
writeTSType (TPrimitive p)                 = writePrimitiveType p
writeTSType (TArray (t1 `TUnion` t2))                 = "(" <> writeTSType t1 <> "[] | " <> writeTSType t2 <> "[])"
writeTSType (TArray t)                 = writeTSType t <> "[]"
writeTSType (a `TUnion` b) = writeTSType a <> " | " <> writeTSType b
writeTSType (TLiteral l) = l

writeInlineInterface :: Members -> Text
writeInlineInterface members =
  writeOpeningBrace Inline
  <> mconcat (intersperse " " $ writeMember Inline <$> members)
  <> writeClosingBrace Inline

writeAnonymousFunctionType :: Arguments -> ReturnType -> Text
writeAnonymousFunctionType args returnType =
  "("
  <> mconcat (intersperse ", " $ writeArgument <$> args)
  <> ") => "
  <> writeTSType returnType

writePrimitiveType :: PrimitiveName -> Text
writePrimitiveType PString  = "string"
writePrimitiveType PNumber  = "number"
writePrimitiveType PBoolean = "boolean"
writePrimitiveType PUnknown = "unknown"
writePrimitiveType PNull    = "null"
writePrimitiveType PVoid    = "void"

writeFunction :: Function -> Text
writeFunction (Function (FunctionName name) arguments returnType) =
  name
  <> "("
  <> mconcat (intersperse ", " $ writeArgument <$> arguments)
  <> "): "
  <> writeTSType returnType
  <> ";"

writeArgument :: Argument -> Text
writeArgument (Argument (ArgumentName name) t) = name <> ": " <> writeTSType t
