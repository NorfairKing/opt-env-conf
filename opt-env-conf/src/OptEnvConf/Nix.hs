{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OptEnvConf.Nix where

import Autodocodec.Nix
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import OptEnvConf.Parser
import OptEnvConf.Setting

renderSettingsNixOSOptions :: forall a. (HasParser a) => Text
renderSettingsNixOSOptions = renderParserNixOSOptions (settingsParser :: Parser a)

renderParserNixOSOptions :: Parser a -> Text
renderParserNixOSOptions = renderExpr . parserNixOSOptionExpr

parserNixOSOptionExpr :: Parser a -> Expr
parserNixOSOptionExpr = withNixArgs . optionsExpr . parserNixOSOptions

parserNixOSOptions :: Parser a -> Map Text Option
parserNixOSOptions = go
  where
    go :: Parser a -> Map Text Option
    go = \case
      ParserPure _ -> M.empty
      ParserAp p1 p2 -> M.unionWith combineOption (go p1) (go p2)
      ParserSelect p1 p2 -> M.unionWith combineOption (go p1) (go p2)
      ParserEmpty _ -> M.empty
      ParserAlt p1 p2 -> M.unionWith combineOption (go p1) (go p2) -- TODO is this right?
      ParserMany p -> go p
      ParserAllOrNothing _ p -> go p
      ParserCheck _ _ _ p -> go p
      ParserCommands _ cs -> M.unionsWith combineOption $ map goCommand cs
      ParserWithConfig _ p1 p2 ->
        -- I'm not sure if we need the first as well because you wouldn't use a
        -- config to load a config but it's technically possible so let's
        -- support it.
        M.unionWith combineOption (go p1) (go p2)
      ParserSetting _ s ->
        let codecTups = maybe [] NE.toList (settingConfigVals s)
         in M.unionsWith combineOption $ flip map codecTups $ \(path, DecodingCodec c) ->
              let mOt = valueCodecNixOptionType c
                  opt =
                    Option
                      { optionType = mOt,
                        optionDescription = T.pack <$> settingHelp s
                      }
                  k = T.intercalate "." (map T.pack (NE.toList path))
               in M.singleton k opt
    combineOption :: Option -> Option -> Option
    combineOption = undefined

    goCommand :: Command a -> Map Text Option
    goCommand = go . commandParser
