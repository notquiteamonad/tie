module TIE.TypeScript where

newtype Document = Document [Namespace] deriving (Eq, Show)

writeDocument :: Document -> Text
writeDocument (Document xs) = mconcat . intersperse "\n" $ write <$> xs

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

write :: Namespace -> Text
write _ = "UNDEFINED"
