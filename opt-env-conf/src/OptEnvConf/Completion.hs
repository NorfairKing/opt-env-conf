{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module OptEnvConf.Completion
  ( generateBashCompletionScript,
    bashCompletionScript,
    generateZshCompletionScript,
    zshCompletionScript,
    generateFishCompletionScript,
    fishCompletionScript,
    runCompletionQuery,
    pureCompletionQuery,
    Completion (..),
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

generateZshCompletionScript :: Path Abs File -> String -> IO ()
generateZshCompletionScript progPath progname = putStrLn $ zshCompletionScript progPath progname

-- | Generated zsh shell completion script
zshCompletionScript :: Path Abs File -> String -> String
zshCompletionScript progPath progname =
  unlines
    [ "#compdef " ++ progname,
      "",
      "local request",
      "local completions",
      "local word",
      "local index=$((CURRENT - 1))",
      "",
      "request=(--query-opt-env-conf-completion --completion-enriched --completion-index $index)",
      "for arg in ${words[@]}; do",
      "  request=(${request[@]} --completion-word $arg)",
      "done",
      "",
      "IFS=$'\\n' completions=($( " ++ fromAbsFile progPath ++ " \"${request[@]}\" ))",
      "",
      "for word in $completions; do",
      "  local -a parts",
      "",
      "  # Split the line at a tab if there is one.",
      "  IFS=$'\\t' parts=($( echo $word ))",
      "",
      "  if [[ -n $parts[2] ]]; then",
      "     if [[ $word[1] == \"-\" ]]; then",
      "       local desc=(\"$parts[1] ($parts[2])\")",
      "       compadd -d desc -- $parts[1]",
      "     else",
      "       local desc=($(print -f  \"%-019s -- %s\" $parts[1] $parts[2]))",
      "       compadd -l -d desc -- $parts[1]",
      "     fi",
      "  else",
      "    compadd -f -- $word",
      "  fi",
      "done"
    ]

generateFishCompletionScript :: Path Abs File -> String -> IO ()
generateFishCompletionScript progPath progname = putStrLn $ fishCompletionScript progPath progname

-- | Generated fish shell completion script
fishCompletionScript :: Path Abs File -> String -> String
fishCompletionScript progPath progname =
  let functionName = progNameToFunctionName progname
   in unlines
        [ " function " ++ functionName,
          "    set -l cl (commandline --tokenize --current-process)",
          "    # Hack around fish issue #3934",
          "    set -l cn (commandline --tokenize --cut-at-cursor --current-process)",
          "    set -l cn (count $cn)",
          "    set -l tmpline --query-opt-env-conf-completion --completion-enriched --completion-index $cn",
          "    for arg in $cl",
          "      set tmpline $tmpline --completion-word $arg",
          "    end",
          "    for opt in (" ++ fromAbsFile progPath ++ " $tmpline)",
          "      if test -d $opt",
          "        echo -E \"$opt/\"",
          "      else",
          "        echo -E \"$opt\"",
          "      end",
          "    end",
          "end",
          "",
          "complete --no-files --command " ++ fromAbsFile progPath ++ " --arguments '(" ++ functionName ++ ")'"
        ]

-- This should be a name that a normal user would never want to define themselves.
progNameToFunctionName :: String -> String
progNameToFunctionName progname = "_opt_env_conf_completion_" ++ toShellFunctionCase progname

runCompletionQuery ::
  Parser a ->
  -- | Enriched
  Bool ->
  -- Where completion is invoked (inbetween arguments)
  Int ->
  -- Provider arguments
  [String] ->
  IO ()
runCompletionQuery parser enriched index ws = do
  let completions = pureCompletionQuery parser index ws
  if enriched
    then
      putStr $
        unlines $
          map
            ( \Completion {..} -> case completionDescription of
                Nothing -> completionSuggestion
                Just d -> completionSuggestion <> "\t" <> d
            )
            completions
    else putStr $ unlines $ map completionSuggestion completions
  pure ()

selectArgs :: Int -> [String] -> (Args, Maybe String)
selectArgs _ix args =
  let selectedArgs = args -- take ix args
   in (parseArgs selectedArgs, NE.last <$> NE.nonEmpty selectedArgs)

data Completion = Completion
  { -- | Completion
    completionSuggestion :: String,
    -- | Description
    completionDescription :: !(Maybe String)
  }
  deriving (Show, Eq)

pureCompletionQuery :: Parser a -> Int -> [String] -> [Completion]
pureCompletionQuery parser ix args =
  -- TODO use the index properly (?)
  fromMaybe [] $ evalState (go parser) selectedArgs
  where
    (selectedArgs, mCursorArg) = selectArgs ix args
    goCommand :: Command a -> State Args (Maybe [Completion])
    goCommand = go . commandParser -- TODO complete with the command
    -- Nothing means "this branch was not valid"
    -- Just means "no completions"
    go :: Parser a -> State Args (Maybe [Completion])
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
      ParserEmpty _ -> pure Nothing
      ParserMany p -> do
        mR <- go p
        case mR of
          Nothing -> pure Nothing
          Just os -> fmap (os ++) <$> go p
      ParserSome p -> do
        mR <- go p
        case mR of
          Nothing -> pure Nothing
          Just os -> fmap (os ++) <$> go p
      ParserAllOrNothing _ p -> go p
      ParserCheck _ _ _ p -> go p
      ParserCommands _ cs -> do
        -- Don't re-use the state accross commands
        Just . concat . catMaybes <$> mapM goCommand cs
      ParserWithConfig _ p1 p2 -> do
        c1 <- go p1
        case c1 of
          Just [] -> go p2
          Just ss -> pure $ Just ss
          Nothing -> pure $ Just []
      ParserSetting _ Setting {..} ->
        if settingHidden
          then pure $ Just []
          else do
            case mCursorArg of
              Nothing -> pure $ Just []
              Just arg -> do
                let suggestions = filter (arg `isPrefixOf`) (map Args.renderDashed settingDasheds)
                let completions =
                      map
                        ( \completionSuggestion ->
                            let completionDescription = settingHelp
                             in Completion {..}
                        )
                        suggestions
                pure $ Just completions

-- ParserAp p1 p2 -> do
--   s1s <- go p1 |> go p2
