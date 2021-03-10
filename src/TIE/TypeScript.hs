module TIE.TypeScript
  ( Argument (..)
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
  , TSType (..)
  , writeDocument
  ) where

import           Data.Text (replace)

newtype Document = Document [Namespace] deriving (Eq, Show)

writeDocument :: Document -> Text
writeDocument (Document xs) = replace "; }" " }" (onePerLine (Indented 0) writeNamespace xs) <> "\n"

data Indentation
  = Indented Int
  | Inline

onePerLine :: Indentation -> (Indentation -> a -> Text) -> [a] -> Text
onePerLine i@(Indented indentation) write xs = renderedIndentation <> (mconcat . intersperse ("\n" <> renderedIndentation) $ fmap (write i) xs)
  where renderedIndentation = toText (replicate indentation ' ')
onePerLine Inline _ _ = error "Internal: onePerLine cannot be called inline"

nextIndentation :: Indentation -> Indentation
nextIndentation (Indented indentation) = Indented $ indentation + 2
nextIndentation Inline                 = Inline

data Namespace = Namespace Exported NamespaceName NamespaceMembers
  deriving (Eq, Show)

data Exported = Exported | Private
  deriving (Eq, Show)

type NamespaceMembers = [NamespaceMember]

data NamespaceMember
  = NMNamespace Namespace
  | NMInterface Interface
  | NMFunction Exported Function
  deriving (Eq, Show)

data Interface = Interface Exported InterfaceName Members
  deriving (Eq, Show)

data Function = Function FunctionName Arguments ReturnType
  deriving (Eq, Show)

type Members = [Member]

data Member
  = MPropertyGroup PropertyName Members
  | MProperty PropertyName TSType
  | MFunction Function
  deriving (Eq, Show)

type Arguments = [Argument]

data Argument = Argument ArgumentName TSType
  deriving (Eq, Show)

type ReturnType = TSType

data TSType
  = TInterface InterfaceName
  | TInlineInterface Members
  | TFunction Arguments ReturnType
  | TPrimitive PrimitiveName
  | TArray TSType
  | TUnion TSType TSType
  deriving (Eq, Show)

instance Semigroup TSType where
  lhs@(TPrimitive PNull `TUnion` _) <> TPrimitive PNull = lhs
  lhs@(_ `TUnion` TPrimitive PNull) <> TPrimitive PNull = lhs
  TPrimitive PNull <> rhs@(TPrimitive PNull `TUnion` _) = rhs
  TPrimitive PNull <> rhs@(_ `TUnion` TPrimitive PNull) = rhs
  TPrimitive PNull <> rhs = rhs `TUnion` TPrimitive PNull
  a <> b = a `TUnion` b

data PrimitiveName
  = PString
  | PNumber
  | PBoolean
  | PUnknown
  | PNull
  | PVoid
  deriving (Eq, Show)

newtype NamespaceName = NamespaceName Text deriving (Eq, Show)

newtype InterfaceName = InterfaceName Text deriving (Eq, Show)

newtype PropertyName = PropertyName Text deriving (Eq, Show)

newtype FunctionName = FunctionName Text deriving (Eq, Show)

newtype ArgumentName = ArgumentName Text deriving (Eq, Show)

writeNamespace :: Indentation -> Namespace -> Text
writeNamespace indentation (Namespace exported (NamespaceName name) members) =
  writeExported exported
  <> "namespace "
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
writeClosingBrace (Indented indentation) =  "\n" <> toText (replicate indentation ' ') <> "}"
writeClosingBrace Inline =  " }"

writeNamespaceMember :: Indentation -> NamespaceMember -> Text
writeNamespaceMember indentation (NMNamespace n) = writeNamespace indentation n
writeNamespaceMember indentation (NMInterface i) = writeInterface indentation i
writeNamespaceMember _ (NMFunction exported f)  = writeExported exported <> "function " <> writeFunction f

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
writeTSType (TInterface (InterfaceName i)) = i
writeTSType (TInlineInterface members) = writeInlineInterface members
writeTSType (TFunction args returnType)                 = writeAnonymousFunctionType args returnType
writeTSType (TPrimitive p)                 = writePrimitiveType p
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
