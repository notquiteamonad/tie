module TIE.TypeScript where

newtype Document = Document [Namespace] deriving (Eq, Show)

writeDocument :: Document -> Text
writeDocument (Document xs) = onePerLine 0 writeNamespace xs

onePerLine :: Int -> (Int -> a -> Text) -> [a] -> Text
onePerLine indentation write xs = renderedIndentation <> (mconcat . intersperse ("\n" <> renderedIndentation) $ fmap (write indentation) xs)
  where renderedIndentation = toText (replicate indentation ' ')

nextIndentation :: Int -> Int
nextIndentation = (+ 2)

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
  | TPrimitive PrimitiveName
  deriving (Eq, Show)

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

writeNamespace :: Int -> Namespace -> Text
writeNamespace indentation (Namespace exported (NamespaceName name) members) =
  writeExported exported
  <> "namespace "
  <> name
  <> writeOpeningBrace
  <> onePerLine (nextIndentation indentation) writeNamespaceMember members
  <> writeClosingBrace indentation

writeExported :: Exported -> Text
writeExported Exported = "export "
writeExported Private  = ""

writeOpeningBrace :: Text
writeOpeningBrace = " {\n"

writeClosingBrace :: Int -> Text
writeClosingBrace indentation =  "\n" <> toText (replicate indentation ' ') <> "}"

writeNamespaceMember :: Int -> NamespaceMember -> Text
writeNamespaceMember indentation (NMNamespace n) = writeNamespace indentation n
writeNamespaceMember indentation (NMInterface i) = writeInterface indentation i
writeNamespaceMember _ (NMFunction exported f)  = writeExported exported <> writeFunction f

writeInterface :: Int -> Interface -> Text
writeInterface indentation (Interface exported (InterfaceName name) members) =
  writeExported exported
  <> "interface "
  <> name
  <> writeOpeningBrace
  <> onePerLine (nextIndentation indentation) writeMember members
  <> writeClosingBrace indentation

writeMember :: Int -> Member -> Text
writeMember indentation (MPropertyGroup name members) = writePropertyGroup indentation name members
writeMember _ (MProperty name t)       = writeProperty name t
writeMember _ (MFunction f)       = writeFunction f

writePropertyGroup :: Int -> PropertyName -> Members -> Text
writePropertyGroup indentation (PropertyName name) members =
  name
  <> ":"
  <> writeOpeningBrace
  <> onePerLine (nextIndentation indentation) writeMember members
  <> writeClosingBrace indentation

writeProperty :: PropertyName -> TSType -> Text
writeProperty (PropertyName name) t = name <> ": " <> writeTSType t <> ";"

writeTSType :: TSType -> Text
writeTSType (TInterface (InterfaceName i)) = i
writeTSType (TPrimitive p)                 = writePrimitiveType p

writePrimitiveType :: PrimitiveName -> Text
writePrimitiveType PString  = "string"
writePrimitiveType PNumber  = "number"
writePrimitiveType PBoolean = "boolean"
writePrimitiveType PUnknown = "unknown"
writePrimitiveType PNull    = "null"
writePrimitiveType PVoid    = "void"

writeFunction :: Function -> Text
writeFunction (Function (FunctionName name) arguments returnType) =
  "function "
  <> name
  <> "("
  <> mconcat (intersperse ", " $ writeArgument <$> arguments)
  <> "): "
  <> writeTSType returnType

writeArgument :: Argument -> Text
writeArgument (Argument (ArgumentName name) t) = name <> ": " <> writeTSType t
