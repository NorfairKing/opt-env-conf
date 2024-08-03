{-# LANGUAGE OverloadedStrings #-}

module OptEnvConf.Output where

import Data.List (intercalate, intersperse)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Version
import GHC.Stack (SrcLoc, prettySrcLoc)
import OptEnvConf.Args (Dashed (..))
import qualified OptEnvConf.Args as Args
import OptEnvConf.Parser
import Text.Colour

stringLines :: String -> [[Chunk]]
stringLines s =
  let ls = T.lines (T.pack s)
   in map (pure . chunk) ls

progNameChunk :: String -> Chunk
progNameChunk = fore yellow . chunk . T.pack

versionChunk :: Version -> Chunk
versionChunk = chunk . T.pack . showVersion

usageChunk :: Chunk
usageChunk = fore cyan "Usage: "

commandChunk :: String -> Chunk
commandChunk = fore magenta . chunk . T.pack

mMetavarChunk :: Maybe Metavar -> Chunk
mMetavarChunk = metavarChunk . fromMaybe "METAVAR"

metavarChunk :: Metavar -> Chunk
metavarChunk = fore yellow . chunk . T.pack

dashedChunks :: [Dashed] -> Maybe [Chunk]
dashedChunks = fmap dashedChunksNE . NE.nonEmpty

dashedChunksNE :: NonEmpty Dashed -> [Chunk]
dashedChunksNE = intersperse (fore cyan "|") . map dashedChunk . NE.toList

dashedChunk :: Dashed -> Chunk
dashedChunk = fore white . chunk . T.pack . Args.renderDashed

envVarChunksNE :: NonEmpty String -> [Chunk]
envVarChunksNE = intersperse (fore cyan "|") . map envVarChunk . NE.toList

envVarChunk :: String -> Chunk
envVarChunk = fore white . chunk . T.pack

confValChunk :: NonEmpty String -> Chunk
confValChunk = fore white . chunk . T.pack . intercalate "." . NE.toList

defaultValueChunks :: String -> [Chunk]
defaultValueChunks val = ["default: ", fore yellow $ chunk $ T.pack val]

exampleValuesChunks :: [String] -> [Chunk]
exampleValuesChunks vals = case vals of
  [] -> []
  [val] -> ["example: ", fore yellow $ chunk $ T.pack val]
  _ -> ["examples: ", fore yellow $ chunk $ T.intercalate ", " $ map (T.pack . show) vals]

mHelpChunk :: Maybe Help -> Chunk
mHelpChunk = maybe (fore red "undocumented") helpChunk

helpChunk :: Help -> Chunk
helpChunk = fore blue . chunk . T.pack

headerChunks :: Text -> [Chunk]
headerChunks t = [fore cyan (chunk t), ":"]

syntaxChunk :: String -> Chunk
syntaxChunk = fore blue . chunk . T.pack

mSrcLocChunk :: Maybe SrcLoc -> Chunk
mSrcLocChunk = maybe "without srcLoc" srcLocChunk

srcLocChunk :: SrcLoc -> Chunk
srcLocChunk = fore cyan . chunk . T.pack . prettySrcLoc

indent :: [[Chunk]] -> [[Chunk]]
indent = map ("  " :)

parenthesise :: [Chunk] -> [Chunk]
parenthesise [c] = [c]
parenthesise cs = fore cyan "(" : cs ++ [fore cyan ")"]

bracketise :: [Chunk] -> [Chunk]
bracketise [c] = [fore cyan "[", c, fore cyan "]"]
bracketise cs = fore cyan "[" : cs ++ [fore cyan "]"]
