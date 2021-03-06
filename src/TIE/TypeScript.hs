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
  | NMFunction Function
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
  <> "{\n"
  <> onePerLine (nextIndentation indentation) (\_ _ -> "UNDEFINED") members
  <> "\n}"

writeExported :: Exported -> Text
writeExported Exported = "export "
writeExported Private  = ""
