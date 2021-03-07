module TIE.Elm.Main (generateInitFunction) where

import qualified Data.Text          as T (isPrefixOf, null, strip)
import           Data.Text.IO       (hGetLine)
import           GHC.IO.Handle      (hIsEOF)
import           TIE.Elm.Expression (readNextExpression)
import           TIE.Elm.Types      (ElmType (CustomType, ElmPrimitiveType),
                                     NeededCustomType, elmTypeFromText,
                                     elmTypeToTSType)
import           TIE.FS             (getMainElmFile)
import           TIE.TypeScript     (Argument (Argument),
                                     ArgumentName (ArgumentName),
                                     Exported (Exported), Function (Function),
                                     FunctionName (FunctionName),
                                     InterfaceName (InterfaceName),
                                     Member (MProperty),
                                     NamespaceMember (NMFunction),
                                     PrimitiveName (PNull),
                                     PropertyName (PropertyName),
                                     TSType (TInlineInterface, TInterface, TPrimitive))

generateInitFunction :: [FilePath] -> IO (NamespaceMember, Maybe NeededCustomType)
generateInitFunction paths = do
  let mainPath = getMainElmFile paths
  putStrLn $ "Reading main from " <> mainPath
  mainDefinition <- withFile mainPath ReadMode (`buildMain` [])
  let flags = elmTypeFromText . fromMaybe (error "Could not read flags type from main definition") .
                readNextExpression . unwords . drop 3 $ words mainDefinition
  pure ( NMFunction Exported $ Function (FunctionName "init")
          [ Argument (ArgumentName "options") . TInlineInterface $
            MProperty (PropertyName "node?") (TInterface (InterfaceName "HTMLElement") <> TPrimitive PNull)
            : case flags of
                ElmPrimitiveType (TPrimitive PNull) -> []
                elmType                  -> [MProperty (PropertyName "flags") $ elmTypeToTSType elmType]
          ]
          (TInterface $ InterfaceName "Elm.Main.App")
       , case flags of
          CustomType _ c -> pure c
          _              -> Nothing
       )

buildMain :: Handle -> [Text] -> IO Text
buildMain h acc = do
  eof <- hIsEOF h
  if eof then
    if null acc then error "Could not find main."
    else wrapUp acc
  else do
    l <- hGetLine h
    if null acc then
      if "main :" `T.isPrefixOf` T.strip l then buildMain h $ l : acc
      else buildMain h acc
    else if T.null (T.strip l) || "main =" `T.isPrefixOf` T.strip l then wrapUp acc
    else buildMain h $ l : acc
  where wrapUp = pure . mconcat . intersperse "\n" . reverse
