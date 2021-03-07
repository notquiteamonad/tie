module TIE.Elm.Main (generateInitFunction) where

import           Data.Text          (isSuffixOf, strip)
import qualified Data.Text          as T (isPrefixOf, null)
import           Data.Text.IO       (hGetLine)
import           GHC.IO.Handle      (hIsEOF)
import           TIE.Elm.Expression (readNextExpression)
import           TIE.Elm.Types      (elmTypeToTSType)
import           TIE.TypeScript

generateInitFunction :: [FilePath] -> IO NamespaceMember
generateInitFunction paths = do
  let mainPath = fromMaybe (error "Could not find a Main.elm in the directory given") .
                  viaNonEmpty head $ filter (\path -> "Main.elm" `isSuffixOf` toText path) paths
  putStrLn $ "Reading main from " <> mainPath
  mainDefinition <- withFile mainPath ReadMode (`buildMain` [])
  let flags = fromMaybe (error "Could not parse flags type from main definition") .
                elmTypeToTSType . fromMaybe (error "Could not read flags type from main definition") .
                readNextExpression . unwords . drop 3 $ words mainDefinition
  pure . NMFunction Exported $ Function (FunctionName "init")
          [ Argument (ArgumentName "options") . TInlineInterface $
            MProperty (PropertyName "node?") (TInterface (InterfaceName "HTMLElement") <> TPrimitive PNull)
            : case flags of
                TPrimitive PNull -> []
                flagsType        -> [MProperty (PropertyName "flags") flagsType]
          ]
          (TInterface $ InterfaceName "Elm.Main.App")

buildMain :: Handle -> [Text] -> IO Text
buildMain h acc = do
  eof <- hIsEOF h
  if eof then
    if null acc then error "Could not find main."
    else wrapUp acc
  else do
    l <- hGetLine h
    if null acc then
      if "main :" `T.isPrefixOf` strip l then buildMain h $ l : acc
      else buildMain h acc
    else if T.null (strip l) || "main =" `T.isPrefixOf` strip l then wrapUp acc
    else buildMain h $ l : acc
  where wrapUp = pure . mconcat . intersperse "\n" . reverse
