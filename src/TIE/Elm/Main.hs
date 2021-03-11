{-# LANGUAGE LambdaCase #-}

module TIE.Elm.Main (generateInitFunction) where

import qualified Data.Text          as T (isPrefixOf, null, strip)
import           Data.Text.IO       (hGetLine)
import           GHC.IO.Handle      (hIsEOF, hSetEncoding)
import           System.IO          (mkTextEncoding)
import           TIE.Elm.Expression (readNextExpression)
import           TIE.Elm.Types      (ElmType (ElmPrimitiveType),
                                     NeededCustomType, elmTypeFromText,
                                     elmTypeToTSType, getCustomTypes)
import           TIE.Response       (Response (..))
import           TIE.TypeScript     (Argument (Argument),
                                     ArgumentName (ArgumentName),
                                     Exported (Exported), Function (Function),
                                     FunctionName (FunctionName),
                                     Member (MProperty),
                                     NamespaceMember (NMFunction),
                                     PrimitiveName (PNull, PVoid),
                                     PropertyName (PropertyName),
                                     ReferenceName (ReferenceName),
                                     TSType (TInlineInterface, TPrimitive, TReference))

generateInitFunction :: FilePath -> IO (Response Text (NamespaceMember, [NeededCustomType]))
generateInitFunction mainPath = do
  putStrLn $ "Reading main from " <> mainPath
  withFile mainPath ReadMode (`buildMain` []) >>= \case
    Ok mainDefinition ->
      case readNextExpression . unwords . drop 3 $ words mainDefinition of
        Just flagsExpression ->
          case elmTypeFromText flagsExpression of
            Ok flags -> pure $ pure
              ( NMFunction Exported $ Function (FunctionName "init")
                  [ Argument (ArgumentName "options") . TInlineInterface $
                    MProperty (PropertyName "node?") (TReference (ReferenceName "HTMLElement") <> TPrimitive PNull)
                    : flagsType
                  ]
                  (TReference $ ReferenceName "Elm.Main.App")
              , getCustomTypes [flags] []
              )
              where flagsType
                      | flags == ElmPrimitiveType (TPrimitive PNull) = []
                      | flags == ElmPrimitiveType (TPrimitive PVoid <> TPrimitive PNull) = []
                      | otherwise                  = [MProperty (PropertyName "flags") $ elmTypeToTSType flags]
            Failed e -> pure $ Failed e
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
        if null acc' then pure . Failed $ "Could not find main's type definition. "
                                     <> "Are you specifying it explicitly?"
        else wrapUp acc'
      else do
        l <- hGetLine h
        if null acc' then
          if "main :" `T.isPrefixOf` T.strip l then go $ l : acc'
          else go acc'
        else if T.null (T.strip l) || "main =" `T.isPrefixOf` T.strip l then wrapUp acc'
        else go $ l : acc'
      where wrapUp = pure . pure . mconcat . intersperse "\n" . reverse
