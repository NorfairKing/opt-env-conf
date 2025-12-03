{-# LANGUAGE OverloadedStrings #-}

module OptEnvConf.Output where

import Data.Char as Char
import Data.List (intercalate, intersperse)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Version
import GHC.Stack (SrcLoc (..), prettySrcLoc)
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

functionChunk :: Text -> Chunk
functionChunk = fore yellow . chunk

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

capabilitiesChunks :: Set Capability -> [Chunk]
capabilitiesChunks caps = case Set.toList caps of
  [] -> []
  cs -> intersperse ", " (map capabilityChunk cs)

capabilityChunk :: Capability -> Chunk
capabilityChunk = fore green . chunk . unCapability

mSrcLocChunk :: Maybe SrcLoc -> Chunk
mSrcLocChunk = maybe "without srcLoc" srcLocChunk

srcLocChunk :: SrcLoc -> Chunk
srcLocChunk = fore cyan . chunk . T.pack . prettySrcLoc . cleanSrcLoc
  where
    -- GHC puts the package hash in there, which may change accross compilation
    -- and that messes with golden tests.
    -- We try to remove the hash from the package here.
    -- The srcLocPackage looks like this:
    -- opt-env-conf-test-0.0.0.3-6OHAhVu967XFT6LS52oRkR-opt-env-conf-test
    -- which looks like:
    -- <name>-<version>-<hash>-<name>
    -- So we take every '-'-separated part until one starts with a number.
    cleanSrcLoc loc = loc {srcLocPackage = cleanPackage (srcLocPackage loc)}
    cleanPackage pkg =
      T.unpack $
        T.intercalate "-" $
          takeWhile (not . maybe False (Char.isDigit . fst) . T.uncons) $
            T.splitOn "-" (T.pack pkg)

indent :: [[Chunk]] -> [[Chunk]]
indent = map ("  " :)

parenthesise :: [Chunk] -> [Chunk]
parenthesise [c] = [c]
parenthesise cs = fore cyan "(" : cs ++ [fore cyan ")"]

bracketise :: [Chunk] -> [Chunk]
bracketise [c] = [fore cyan "[", c, fore cyan "]"]
bracketise cs = fore cyan "[" : cs ++ [fore cyan "]"]
