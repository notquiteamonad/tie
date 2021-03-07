{-# LANGUAGE NamedFieldPuns #-}

module TIE.Elm.Ports (generatePortProperties) where

import           TIE.Elm.Types
import           TIE.TypeScript

data Port = Port
  { direction :: PortDirection
  , name      :: PortName
  , elmType   :: ElmType
  } deriving (Eq, Show)

data PortDirection = In | Out deriving (Eq, Show)

newtype PortName = PortName {unPortName :: Text} deriving (Eq, Show)

generatePortProperties :: [FilePath] -> IO (Members, [NeededCustomType])
generatePortProperties paths = do
  let ports = [ Port In (PortName "gimmeCreds") (elmTypeFromText "Maybe Credentials")
              , Port Out (PortName "haveNothing") (elmTypeFromText "()")
              ]
  pure
    ( toMember <$> ports
    , getCustomTypes ports []
    )

toMember :: Port -> Member
toMember Port {direction, name, elmType} =
  let functionName = FunctionName $ if direction == In then "send" else "subscribe"
      args = case direction of
        In  -> [ Argument (ArgumentName "data") (elmTypeToTSType elmType) ]
        Out -> [ Argument (ArgumentName "callback") $ callbackTypeFor elmType ]
  in
  MPropertyGroup (PropertyName $ unPortName name) [ MFunction . Function functionName args $ TPrimitive PVoid ]

getCustomTypes :: [Port] -> [NeededCustomType] -> [NeededCustomType]
getCustomTypes [] acc = acc
getCustomTypes (Port {elmType}:ps) acc =
  case elmType of
    CustomType _ nct -> getCustomTypes ps $ nct : acc
    _                -> getCustomTypes ps acc

callbackTypeFor :: ElmType -> TSType
callbackTypeFor t = TFunction [Argument (ArgumentName "data") $ elmTypeToTSType t] $ TPrimitive PVoid
