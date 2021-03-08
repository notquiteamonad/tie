{-# LANGUAGE LambdaCase #-}

module TIE.Elm.Main (generateInitFunction) where

import qualified Data.Text          as T (isPrefixOf, null, strip)
import           Data.Text.IO       (hGetLine)
import           GHC.IO.Handle      (hIsEOF, hSetEncoding)
import           System.IO          (mkTextEncoding)
import           TIE.Elm.Expression (readNextExpression)
import           TIE.Elm.Types      (ElmType (CustomType, ElmPrimitiveType),
                                     NeededCustomType, elmTypeFromText,
                                     elmTypeToTSType)
import           TIE.Response       (Response (..))
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

generateInitFunction :: FilePath -> IO (Response Text (NamespaceMember, Maybe NeededCustomType))
generateInitFunction mainPath = do
  putStrLn $ "Reading main from " <> mainPath
  withFile mainPath ReadMode (`buildMain` []) >>= \case
    Ok mainDefinition ->
      case readNextExpression . unwords . drop 3 $ words mainDefinition of
        Just flagsExpression ->
          let flags = elmTypeFromText flagsExpression
          in pure $ pure
            ( NMFunction Exported $ Function (FunctionName "init")
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
        Nothing -> pure $ Failed "Could not read flags type from main definition"
    Failed e -> pure $ Failed e

buildMain :: Handle -> [Text] -> IO (Response Text Text)
buildMain h acc = do
  enc <- mkTextEncoding "UTF-8//IGNORE"
  hSetEncoding h enc
  go acc
  where
    go acc' = do
      eof <- hIsEOF h
      if eof then
        if null acc' then pure $ Failed "Could not find main."
        else wrapUp acc'
      else do
        l <- hGetLine h
        if null acc' then
          if "main :" `T.isPrefixOf` T.strip l then go $ l : acc'
          else go acc'
        else if T.null (T.strip l) || "main =" `T.isPrefixOf` T.strip l then wrapUp acc'
        else go $ l : acc'
      where wrapUp = pure . pure . mconcat . intersperse "\n" . reverse
