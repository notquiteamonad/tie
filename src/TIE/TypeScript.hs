module TIE.TypeScript
  ( AliasName (..)
  , Argument (..)
  , ArgumentName (..)
  , Document (..)
  , Exported (..)
  , Function (..)
  , FunctionName (..)
  , Interface (..)
  , InterfaceName (..)
  , Members
  , Member (..)
  , Namespace (..)
  , NamespaceMember (..)
  , NamespaceName (..)
  , PrimitiveName (..)
  , PropertyName (..)
  , ReferenceName (..)
  , TSType (..)
  , writeDocument
  ) where

import           Data.Text (replace)

newtype Document = Document [Namespace] deriving (Eq, Ord, Show)

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

data Namespace = Namespace NamespaceName NamespaceMembers
  deriving (Eq, Ord, Show)

data Exported = Exported | Private
  deriving (Eq, Ord, Show)

type NamespaceMembers = [NamespaceMember]

data NamespaceMember
  = NMNamespace Exported Namespace
  | NMAlias AliasName TSType
  | NMInterface Interface
  | NMFunction Exported Function
  deriving (Eq, Ord, Show)

data Interface = Interface Exported InterfaceName Members
  deriving (Eq, Ord, Show)

data Function = Function FunctionName Arguments ReturnType
  deriving (Eq, Ord, Show)

type Members = [Member]

data Member
  = MPropertyGroup PropertyName Members
  | MProperty PropertyName TSType
  | MFunction Function
  deriving (Eq, Ord, Show)

type Arguments = [Argument]

data Argument = Argument ArgumentName TSType
  deriving (Eq, Ord, Show)

type ReturnType = TSType

data TSType
  = TReference ReferenceName
  | TInlineInterface Members
  | TFunction Arguments ReturnType
  | TPrimitive PrimitiveName
  | TArray TSType
  | TUnion TSType TSType
  deriving (Ord, Show)

instance Eq TSType where
  TReference a == TReference b = a == b
  TInlineInterface a == TInlineInterface b = a == b
  TFunction a b == TFunction c d = a == c && b == d
  TPrimitive a == TPrimitive b = a == b
  TArray a == TArray b = a == b
  TUnion a b == TUnion c d = (a == c && b == d) || (a == d && b == c)
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

data PrimitiveName
  = PString
  | PNumber
  | PBoolean
  | PUnknown
  | PNull
  | PVoid
  deriving (Eq, Ord, Show)

newtype NamespaceName = NamespaceName Text deriving (Eq, Ord, Show)

newtype InterfaceName = InterfaceName Text deriving (Eq, Ord, Show)

newtype ReferenceName = ReferenceName Text deriving (Eq, Ord, Show)

newtype AliasName = AliasName Text deriving (Eq, Ord, Show)

newtype PropertyName = PropertyName Text deriving (Eq, Ord, Show)

newtype FunctionName = FunctionName Text deriving (Eq, Ord, Show)

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
