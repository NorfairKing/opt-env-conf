{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module OptEnvConf.Completion
  ( generateBashCompletionScript,
    runBashCompletionQuery,
    pureCompletionQuery,
  )
where

import Control.Monad.State
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import OptEnvConf.Args as Args
import OptEnvConf.Casing
import OptEnvConf.Parser
import OptEnvConf.Setting
import Path

generateBashCompletionScript :: Path Abs File -> String -> IO ()
generateBashCompletionScript progPath progname = putStrLn $ bashCompletionScript progPath progname

-- | Generated bash shell completion script
bashCompletionScript :: Path Abs File -> String -> String
bashCompletionScript progPath progname =
  let functionName = progNameToFunctionName progname
   in unlines
        [ functionName ++ "()",
          "{",
          "    local CMDLINE",
          "    local IFS=$'\\n'",
          "    CMDLINE=(--query-opt-env-conf-completion)",
          "    CMDLINE+=(--completion-index $COMP_CWORD)",
          "",
          "    for arg in ${COMP_WORDS[@]}; do",
          "        CMDLINE=(${CMDLINE[@]} --completion-word $arg)",
          "    done",
          "",
          "    COMPREPLY=( $(" ++ fromAbsFile progPath ++ " \"${CMDLINE[@]}\") )",
          "    echo \"${COMPREPLY[@]}\" > hm.log",
          "}",
          "",
          "complete -o filenames -F " ++ functionName ++ " " ++ progname
        ]

-- This should be a name that a normal user would never want to define themselves.
progNameToFunctionName :: String -> String
progNameToFunctionName progname = "_opt_env_conf_completion_" ++ toShellFunctionCase progname

runBashCompletionQuery ::
  Parser a ->
  -- Where completion is invoked (inbetween arguments)
  Int ->
  -- Provider arguments
  [String] ->
  IO ()
runBashCompletionQuery parser index ws = do
  let completions = pureCompletionQuery parser index ws
  putStr $ unlines completions
  pure ()

selectArgs :: Int -> [String] -> (Args, Maybe String)
selectArgs _ix args =
  let selectedArgs = args -- take ix args
   in (parseArgs selectedArgs, NE.last <$> NE.nonEmpty selectedArgs)

pureCompletionQuery :: Parser a -> Int -> [String] -> [String]
pureCompletionQuery parser ix args =
  -- TODO use the index properly (?)
  fromMaybe [] $ evalState (go parser) selectedArgs
  where
    (selectedArgs, mCursorArg) = selectArgs ix args
    goCommand :: Command a -> State Args (Maybe [String])
    goCommand = go . commandParser -- TODO complete with the command
    -- Nothing means "this branch was not valid"
    -- Just means "no completions"
    go :: Parser a -> State Args (Maybe [String])
    go = \case
      ParserPure _ -> pure $ Just []
      ParserAp p1 p2 -> do
        c1 <- go p1
        case c1 of
          Just [] -> go p2
          Just ss -> pure $ Just ss
          Nothing -> pure $ Just []
      ParserAlt p1 p2 -> do
        s1s <- go p1
        s2s <- go p2
        pure $ (++) <$> s1s <*> s2s
      ParserSelect p1 p2 -> do
        c1 <- go p1
        case c1 of
          Just [] -> go p2
          Just ss -> pure $ Just ss
          Nothing -> pure $ Just []
      ParserEmpty -> pure Nothing
      ParserMany p -> do
        mR <- go p
        case mR of
          Nothing -> pure Nothing
          Just os -> fmap (os ++) <$> go p
      ParserCheck _ _ p -> go p
      ParserCommands cs -> do
        -- Don't re-use the state accross commands
        Just . concat . catMaybes <$> mapM goCommand cs
      ParserWithConfig _ p -> go p
      ParserSetting _ Setting {..} ->
        if settingHidden
          then pure $ Just []
          else do
            case mCursorArg of
              Nothing -> pure $ Just []
              Just arg -> do
                pure $ Just $ filter (arg `isPrefixOf`) (map Args.renderDashed settingDasheds)

-- ParserAp p1 p2 -> do
--   s1s <- go p1 |> go p2
