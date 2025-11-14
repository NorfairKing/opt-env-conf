{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OptEnvConf.Nix where

import Autodocodec
import Autodocodec.Nix
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import OptEnvConf.Parser
import OptEnvConf.Setting

renderSettingsNixOptions :: forall a. (HasParser a) => Text
renderSettingsNixOptions = renderParserNixOptions (settingsParser :: Parser a)

renderParserNixOptions :: Parser a -> Text
renderParserNixOptions = renderExpr . parserNixOptionExpr

parserNixOptionExpr :: Parser a -> Expr
parserNixOptionExpr = withNixArgs . optionsExpr . parserNixOptions

parserNixOptions :: Parser a -> Map Text Option
parserNixOptions = go
  where
    go :: Parser a -> Map Text Option
    go = \case
      ParserPure _ -> M.empty
      ParserAp p1 p2 -> M.unionWith combineOption (go p1) (go p2)
      ParserSelect p1 p2 -> M.unionWith combineOption (go p1) (go p2)
      ParserEmpty _ -> M.empty
      ParserAlt p1 p2 -> M.unionWith combineOption (go p1) (go p2) -- TODO is this right?
      ParserMany _ p -> go p
      ParserSome _ p -> go p
      ParserAllOrNothing _ p -> go p
      ParserCheckPure _ _ _ p -> go p
      ParserCheckIO _ _ _ p -> go p
      ParserRequireCapability _ _ p -> go p
      ParserCommands _ _ cs -> M.unionsWith combineOption $ map goCommand cs
      ParserWithConfig _ p1 p2 ->
        -- I'm not sure if we need the first as well because you wouldn't use a
        -- config to load a config but it's technically possible so let's
        -- support it.
        M.unionWith combineOption (go p1) (go p2)
      ParserSetting _ s ->
        let codecTups = maybe [] NE.toList (settingConfigVals s)
         in M.unionsWith combineOption $ flip map codecTups $ \ConfigValSetting {..} ->
              let go' :: NonEmpty Text -> Map Text Option
                  go' (p :| ps) = case NE.nonEmpty ps of
                    Nothing ->
                      let oc =
                            maybe
                              (optionalFieldWith' p configValSettingCodec)
                              (optionalFieldWith p configValSettingCodec)
                              (T.pack <$> settingHelp s)
                       in objectCodecNixOptions oc
                    Just rest ->
                      let m = go' rest
                       in M.singleton p $ emptyOption {optionType = Just (OptionTypeSubmodule m)}
               in go' $ NE.map T.pack configValSettingPath
    combineOption :: Option -> Option -> Option
    combineOption o1 o2 = case (optionType o1, optionType o2) of
      (Nothing, _) -> o2
      (Just ot1, Nothing) -> o2 {optionType = Just ot1}
      (Just ot1, Just ot2) -> o2 {optionType = Just $ combineOptionType ot1 ot2}

    combineOptionType :: OptionType -> OptionType -> OptionType
    combineOptionType ot1 ot2 = simplifyOptionType $ case (ot1, ot2) of
      (OptionTypeSubmodule m1, OptionTypeSubmodule m2) -> OptionTypeSubmodule $ M.unionWith combineOption m1 m2
      _ -> OptionTypeOneOf [ot1, ot2]
    goCommand :: Command a -> Map Text Option
    goCommand = go . commandParser
