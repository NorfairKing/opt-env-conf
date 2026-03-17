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
    evalCompletions,
    evalCompletion,
    Suggestion (..),
    evalSuggestion,
  )
where

import Control.Monad
import Control.Monad.State
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.String
import OptEnvConf.Args as Args
import OptEnvConf.Casing
import OptEnvConf.Completer
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
      "     elif [[ $parts[3] == 'N' ]]; then",
      "       local desc=($(print -f  \"%-019s -- %s\" $parts[1] $parts[2]))",
      "       compadd -f -l -S '' -d desc -- $parts[1]",
      "     else",
      "       local desc=($(print -f  \"%-019s -- %s\" $parts[1] $parts[2]))",
      "       compadd -f -l -d desc -- $parts[1]",
      "     fi",
      "  else",
      "    if [[ $parts[3] == 'N' ]]; then",
      "      compadd -f -S '' -- $parts[1]",
      "    else",
      "      compadd -f -- $parts[1]",
      "    fi",
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
          "      set -l val (string split \\t -- $opt)[1]",
          "      if test -d $val",
          "        echo -E \"$val/\"",
          "      else",
          "        echo -E \"$val\"",
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
  -- | Where completion is invoked (inbetween arguments)
  Int ->
  -- Provider arguments
  [String] ->
  IO ()
runCompletionQuery parser enriched index' ws' = do
  -- Which index and args are passed here is a bit tricky.
  -- Some examples:
  --
  -- progname <tab>      -> (1, ["progname"])
  -- progname <tab>-     -> (1, ["progname", "-"])
  -- progname -<tab>     -> (1, ["progname", "-"])
  -- progname -<tab>-    -> (1, ["progname", "--"])
  -- progname - <tab>    -> (2, ["progname", "-"])
  --
  -- We use 'drop 1' here because we don't care about the progname anymore.
  let index = pred index'
  let ws = drop 1 ws'
  let arg = fromMaybe "" $ listToMaybe $ drop index ws
  let completions = pureCompletionQuery parser index ws
  evaluatedCompletions <- evalCompletions arg completions
  -- You can use this for debugging inputs:
  -- import System.IO
  -- hPutStrLn stderr $ show (enriched, index, ws)
  -- hPutStrLn stderr $ show evaluatedCompletions
  if enriched
    then
      putStr $
        unlines $
          map
            ( \Completion {..} ->
                let val = completionResultValue completionSuggestion
                    notFinal = completionResultFinality completionSuggestion == CompletionNotFinal
                 in case (notFinal, completionDescription) of
                      (False, Nothing) -> val
                      (False, Just d) -> val <> "\t" <> d
                      (True, Nothing) -> val <> "\t\tN"
                      (True, Just d) -> val <> "\t" <> d <> "\tN"
            )
            evaluatedCompletions
    else putStr $ unlines $ map (completionResultValue . completionSuggestion) evaluatedCompletions
  pure ()

-- Because the first arg has already been skipped we get input like this here:
--
-- progname <tab>      -> (0, [])
-- progname <tab>-     -> (0, ["-"])
-- progname -<tab>     -> (0, ["-"])
-- progname -<tab>-    -> (0, ["--"])
-- progname - <tab>    -> (1, ["-"])
selectArgs :: Int -> [String] -> (Args, Maybe String)
selectArgs ix args =
  ( parseArgs $ take ix args,
    NE.head <$> NE.nonEmpty (drop ix args)
  )

data Completion a = Completion
  { -- | Completion
    completionSuggestion :: !a,
    -- | Description
    completionDescription :: !(Maybe String)
  }
  deriving (Show, Eq, Ord)

instance (IsString str) => IsString (Completion str) where
  fromString s =
    Completion
      { completionSuggestion = fromString s,
        completionDescription = Nothing
      }

evalCompletions :: String -> [Completion Suggestion] -> IO [Completion CompletionResult]
evalCompletions arg = fmap concat . mapM (evalCompletion arg)

evalCompletion :: String -> Completion Suggestion -> IO [Completion CompletionResult]
evalCompletion arg c = do
  rs <- evalSuggestion arg (completionSuggestion c)
  pure $ map (\r -> c {completionSuggestion = r}) rs

data Suggestion
  = SuggestionBare !String
  | SuggestionCompleter !Completer

-- For tidier tests
instance IsString Suggestion where
  fromString = SuggestionBare

evalSuggestion :: String -> Suggestion -> IO [CompletionResult]
evalSuggestion arg = \case
  SuggestionBare s -> pure $ filter ((arg `isPrefixOf`) . completionResultValue) [finalResult s]
  SuggestionCompleter (Completer act) -> act arg

pureCompletionQuery :: Parser a -> Int -> [String] -> [Completion Suggestion]
pureCompletionQuery parser ix args =
  fromMaybe [] $ evalState (go parser) selectedArgs
  where
    (selectedArgs, mCursorArg) = selectArgs ix args
    -- Complete within a command's parser. The command name itself is
    -- already handled in the ParserCommands branch (argsAtEnd case).
    goCommand :: Command a -> State Args (Maybe [Completion Suggestion])
    goCommand = go . commandParser
    combineOptions = Just . concat . catMaybes

    tryOrRestore :: State Args (Maybe a) -> State Args (Maybe a)
    tryOrRestore func = do
      before <- get
      mA <- func
      case mA of
        Nothing -> do
          put before
          pure Nothing
        Just a -> pure (Just a)

    -- Completions for many/some: try the parser repeatedly.
    -- Each iteration either advances the args state (consuming input)
    -- or produces end-of-input suggestions. We keep completions from
    -- the iteration that is at the cursor position (the last one that
    -- advanced state, or the first if none advance).
    manyCompletions :: Parser x -> State Args (Maybe [Completion Suggestion])
    manyCompletions p = do
      before <- get
      mR <- go p
      case mR of
        Nothing -> pure Nothing
        Just os -> do
          after <- get
          if after == before
            then -- State did not advance; return these completions.
              pure $ Just os
            else -- State advanced: something was consumed. Try the
            -- next iteration. Its completions supersede ours
            -- only if it also has a valid result.
              do
                mMore <- manyCompletions p
                case mMore of
                  Nothing -> pure $ Just os
                  Just more
                    -- If the next iteration only produced stale
                    -- dashed suggestions (state didn't advance
                    -- further), prefer our completions which came
                    -- from the advancing iteration.
                    | null os -> pure $ Just more
                    | otherwise -> pure $ Just os

    orCompletions :: Parser x -> Parser y -> State Args (Maybe [Completion Suggestion])
    orCompletions p1 p2 = do
      p1s <- tryOrRestore $ go p1
      p2s <- tryOrRestore $ go p2
      pure $ case (p1s, p2s) of
        (Nothing, Nothing) -> Nothing
        (Just cs, Nothing) -> Just cs
        (Nothing, Just cs) -> Just cs
        (Just cs1, Just cs2) -> Just $ cs1 ++ cs2

    andCompletions :: Parser x -> Parser y -> State Args (Maybe [Completion Suggestion])
    andCompletions p1 p2 = do
      p1s <- tryOrRestore $ go p1
      case p1s of
        Nothing -> pure Nothing
        Just cs1 -> do
          p2s <- tryOrRestore $ go p2
          pure $ case p2s of
            Nothing -> Nothing
            Just cs2 -> pure $ cs1 ++ cs2

    -- Nothing means "this branch was not valid"
    -- Just [] means "no completions"
    go :: Parser a -> State Args (Maybe [Completion Suggestion])
    go = \case
      ParserPure _ -> pure $ Just []
      -- Parse both and combine the result
      ParserAp p1 p2 -> andCompletions p1 p2
      -- Parse either: either completions are valid
      ParserAlt p1 p2 -> orCompletions p1 p2
      ParserSelect p1 p2 -> andCompletions p1 p2
      ParserEmpty _ -> pure Nothing
      ParserMany _ p -> manyCompletions p
      ParserSome _ p -> manyCompletions p
      ParserAllOrNothing _ p -> go p
      ParserCheck _ _ _ _ p -> go p
      ParserWithConfig _ p1 p2 ->
        -- The config-file auto-completion is probably less important, we put it second.
        andCompletions p2 p1
      ParserCommands _ mDefault cs -> do
        let mDefaultCommand = do
              d <- mDefault
              find ((== d) . commandArg) cs
        as <- get
        let possibilities = Args.consumeArgument as
        -- First, try matching an explicit command name.
        explicitCommandCompletions <-
          fmap combineOptions $ forM possibilities $ \(mArg, rest) -> do
            case mArg of
              Nothing -> do
                if argsAtEnd rest
                  then do
                    let arg = fromMaybe "" mCursorArg
                    let matchingCommands = filter ((arg `isPrefixOf`) . commandArg) cs
                    pure $
                      Just $
                        map
                          ( \Command {..} ->
                              Completion
                                { completionSuggestion = SuggestionBare commandArg,
                                  completionDescription = Just commandHelp
                                }
                          )
                          matchingCommands
                  else -- consumeArgument offered "don't consume" but there
                  -- are still live args remaining. This possibility is
                  -- invalid for commands: if we didn't consume a command
                  -- name then the remaining args have nowhere to go.
                    pure Nothing
              Just arg ->
                case find ((== arg) . commandArg) cs of
                  Just c -> do
                    put rest
                    goCommand c
                  Nothing -> pure Nothing
        -- If there is a default command, also try completing within
        -- the default command's parser, since that is what would run
        -- if the user provides no command.
        case mDefaultCommand of
          Nothing -> pure explicitCommandCompletions
          Just dc -> do
            defaultCompletions <- tryOrRestore $ goCommand dc
            case defaultCompletions of
              Nothing -> pure explicitCommandCompletions
              Just dcs
                -- If no args were consumed (we were already at end),
                -- combine the explicit command listing with the default
                -- command's completions.
                | argsAtEnd as -> pure $ combineOptions [explicitCommandCompletions, Just dcs]
                | otherwise -> do
                    -- The default command consumed args, so its
                    -- completions are valid.  But we must restore
                    -- the state: the consumed args may also be
                    -- intended for sibling parsers in an
                    -- applicative (<*>), e.g. an option like
                    -- --archive-dir that the default command
                    -- swallowed as a positional argument.
                    put as
                    pure $ Just dcs
      ParserSetting _ Setting {..} -> do
        let arg = fromMaybe "" mCursorArg
        let completionDescription = settingHelp
        let completeWithCompleter = pure $ Just $ maybeToList $ do
              c <- settingCompleter
              let completionSuggestion = SuggestionCompleter c
              pure Completion {..}
        let completeWithCompleterAtEnd = do
              as <- get
              if argsAtEnd as then completeWithCompleter else pure $ Just []
        let completeWithDasheds = do
              let isLong = \case
                    DashedLong _ -> True
                    DashedShort _ -> False
              let favorableDasheds = if any isLong settingDasheds then filter isLong settingDasheds else settingDasheds
              let suggestions = filter (arg `isPrefixOf`) (map Args.renderDashed favorableDasheds)
              let completions =
                    map
                      ( ( \completionSuggestion ->
                            Completion {..}
                        )
                          . SuggestionBare
                      )
                      suggestions
              pure $ Just completions
        if settingHidden
          then pure $ Just []
          else do
            as <- get
            if settingTryArgument
              then do
                let possibilities = Args.consumeArgument as
                -- Try all possible consumptions of the argument.
                -- If any possibility actually consumes a value, prefer
                -- that over the "don't consume" fallback, because a
                -- consumed value means the user already provided input.
                case filter (isJust . fst) possibilities of
                  (_, as') : _ -> do
                    put as'
                    pure $ Just []
                  [] ->
                    -- No possibility consumed a value.  This is either
                    -- because there are no args at all (the [] case from
                    -- consumeArgument) or because only the consume-nothing
                    -- fallback matched.  In both cases, offer the
                    -- completer if we are at the end.
                    case possibilities of
                      [] -> completeWithCompleterAtEnd
                      (_, as') : _ -> do
                        put as'
                        completeWithCompleterAtEnd
              else
                if isJust settingSwitchValue
                  then do
                    -- Try to parse the switch first, so we don't suggest it if
                    -- it's already been parsed.
                    case Args.consumeSwitch settingDasheds as of
                      Nothing ->
                        -- A switch can be anywhere, doesn't need to be at the end.
                        completeWithDasheds
                      Just as' -> do
                        put as'
                        pure $ Just []
                  else do
                    if settingTryOption
                      then do
                        -- First we try to consume the option so we don't suggest it if it's already been parsed
                        case Args.consumeOption settingDasheds as of
                          Just (_, as') -> do
                            put as'
                            pure $ Just []
                          Nothing -> do
                            if argsAtEnd as
                              then completeWithDasheds
                              else do
                                -- If we're not at the end, we may be between an option's
                                -- dashed an the option value being tab-completed In that case
                                -- we need to parse the dashed as normal and check if that
                                -- brings us to the end.
                                --
                                -- We use 'consumeSwitch' to consume the dashed part of
                                -- the option because consumeOption would try to
                                -- consume the option argument too.
                                case Args.consumeSwitch settingDasheds as of
                                  Nothing -> pure $ Just []
                                  Just as' -> do
                                    put as'
                                    completeWithCompleterAtEnd
                      else do
                        -- We can't auto-complete settings parsed from env vars
                        -- or config values, but this path is still valid.
                        --
                        -- If we checked whether the env var is set or the
                        -- config val is present, we could return Nothing when
                        -- they are absent. That would let alternatives reject
                        -- this branch, improving completions when one branch
                        -- is env/conf-only and the other has args/options.
                        -- This would require IO or an environment parameter.
                        pure $ Just []
