{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module OptEnvConf.Main
  ( runSettingsParser,
    runParser,
    internalParser,
  )
where

import qualified Data.Text as T
import Data.Version
import OptEnvConf.Args as Args
import OptEnvConf.Capability
import OptEnvConf.Check
import OptEnvConf.Completion
import OptEnvConf.Doc
import qualified OptEnvConf.EnvMap as EnvMap
import OptEnvConf.Error
import OptEnvConf.Lint
import OptEnvConf.Nix
import OptEnvConf.Parser
import OptEnvConf.Reader
import OptEnvConf.Run
import OptEnvConf.Setting
import OptEnvConf.Terminal (getTerminalCapabilitiesFromHandle)
import Path
import System.Environment (getArgs, getEnvironment, getProgName)
import System.Exit
import System.IO
import Text.Colour

-- | Run 'runParser' on your @Settings@' type's 'settingsParser'.
--
-- __This is most likely the function you want to be using.__
runSettingsParser ::
  (HasParser a) =>
  -- | Program version, get this from Paths_your_package_name
  Version ->
  -- | Program description
  String ->
  IO a
runSettingsParser version progDesc =
  runParser version progDesc settingsParser

-- | Run a parser
--
-- This function with exit on:
--
--     * Parse failure: show a nice error message.
--     * @-h|--help@: Show help text
--     * @--version@: Show version information
--     * @--render-man-page@: Render a man page
--     * @--bash-completion-script@: Render a bash completion script
--     * @--zsh-completion-script@: Render a zsh completion script
--     * @--fish-completion-script@: Render a fish completion script
--     * @query-opt-env-conf-completion@: Perform a completion query
--
-- This gets the arguments and environment variables from the current process.
runParser ::
  -- | Program version, get this from Paths_your_package_name
  Version ->
  -- | Program description
  String ->
  Parser a ->
  IO a
runParser version progDesc p = do
  completeEnv <- getEnvironment
  let envVars = EnvMap.parse completeEnv

  case lintParser p of
    Just errs -> do
      tc <- getTerminalCapabilitiesFromHandle stderr
      hPutChunksLocaleWith tc stderr $ renderLintErrors errs
      exitFailure
    Nothing -> do
      let docs = parserDocs p

      allArgs <- getArgs
      let (debugMode, args) = consumeDebugMode allArgs

      mDebugMode <-
        if debugMode
          then Just <$> getTerminalCapabilitiesFromHandle stderr
          else pure Nothing

      let (helpMode, args') = consumeHelpMode args

      if helpMode
        then do
          progname <- getProgName
          errOrDocs <- runHelpParser mDebugMode (Args.parseArgs args') p
          case errOrDocs of
            Left errs -> do
              stderrTc <- getTerminalCapabilitiesFromHandle stderr
              hPutChunksLocaleWith stderrTc stderr $ renderErrors errs
              exitFailure
            Right mCommandDoc -> do
              tc <- getTerminalCapabilitiesFromHandle stdout
              hPutChunksLocaleWith tc stdout $ case mCommandDoc of
                Nothing -> renderHelpPage progname version progDesc docs
                Just (path, cDoc) -> renderCommandHelpPage progname path cDoc
              exitSuccess
        else do
          let (capabilities, args'') = consumeCapabilities args'
          let (checkMode, args''') = consumeCheckMode args''

          let readyArgs = Args.parseArgs args'''

          let mConfig = Nothing -- We start with no config loaded.
          if checkMode
            then runSettingsCheck capabilities p readyArgs envVars mConfig
            else do
              let p' = internalParser p
              errOrResult <-
                runParserOn
                  capabilities
                  mDebugMode
                  p'
                  readyArgs
                  envVars
                  mConfig
              case errOrResult of
                Left errs -> do
                  tc <- getTerminalCapabilitiesFromHandle stderr
                  hPutChunksLocaleWith tc stderr $ renderErrors errs
                  exitFailure
                Right i -> case i of
                  ShowVersion -> do
                    progname <- getProgName
                    tc <- getTerminalCapabilitiesFromHandle stdout
                    hPutChunksLocaleWith tc stdout $ renderVersionPage progname version
                    exitSuccess
                  RenderMan -> do
                    progname <- getProgName
                    tc <- getTerminalCapabilitiesFromHandle stdout
                    hPutChunksLocaleWith tc stdout $ renderManPage progname version progDesc docs
                    exitSuccess
                  RenderDocumentation -> do
                    progname <- getProgName
                    tc <- getTerminalCapabilitiesFromHandle stdout
                    hPutChunksLocaleWith tc stdout $ renderReferenceDocumentation progname docs
                    exitSuccess
                  RenderNixosOptions -> do
                    putStrLn $ T.unpack $ renderParserNixOptions p'
                    exitSuccess
                  BashCompletionScript progPath -> do
                    progname <- getProgName
                    generateBashCompletionScript progPath progname
                    exitSuccess
                  ZshCompletionScript progPath -> do
                    progname <- getProgName
                    generateZshCompletionScript progPath progname
                    exitSuccess
                  FishCompletionScript progPath -> do
                    progname <- getProgName
                    generateFishCompletionScript progPath progname
                    exitSuccess
                  CompletionQuery enriched index ws -> do
                    runCompletionQuery p' enriched index ws
                    exitSuccess
                  ParsedNormally a -> pure a

-- We use [String] instead of [Args] because we want to remove these args, and act on them, before any real arg parsing happens.
consumeExactArg :: String -> [String] -> (Bool, [String])
consumeExactArg arg = go
  where
    go = \case
      [] -> (False, [])
      (x : xs)
        | x == arg -> (True, xs)
        | otherwise ->
            let (found, rest) = go xs
             in (found, x : rest)

consumeDebugMode :: [String] -> (Bool, [String])
consumeDebugMode = consumeExactArg "--debug-optparse"

-- Note: Here we only consume exact -h as an arg, not -h as part of another arg like -hu
consumeHelpMode :: [String] -> (Bool, [String])
consumeHelpMode as =
  let (found, as') = consumeExactArg "--help" as
   in if found
        then (True, as')
        else consumeExactArg "-h" as'

consumeCheckMode :: [String] -> (Bool, [String])
consumeCheckMode = consumeExactArg "--run-settings-check"

consumeCapabilities :: [String] -> (Capabilities, [String])
consumeCapabilities = go allCapabilities
  where
    go :: Capabilities -> [String] -> (Capabilities, [String])
    go caps = \case
      [] -> (caps, [])
      (x : xs) ->
        let t = T.pack x
         in case T.stripPrefix "--settings-capabilities-disable-" t of
              Just capName -> go (disableCapability (Capability capName) caps) xs
              Nothing -> case T.stripPrefix "--settings-capabilities-enable-" t of
                Just capName -> go (enableCapability (Capability capName) caps) xs
                Nothing ->
                  let (finalCaps, rest) = go caps xs
                   in (finalCaps, x : rest)

-- Internal structure to help us do what the framework
-- is supposed to.
data Internal a
  = ShowVersion
  | RenderMan
  | RenderDocumentation
  | RenderNixosOptions
  | BashCompletionScript (Path Abs File)
  | ZshCompletionScript (Path Abs File)
  | FishCompletionScript (Path Abs File)
  | CompletionQuery
      -- Enriched
      !Bool
      -- Index
      !Int
      -- Args
      ![String]
  | ParsedNormally !a

internalParser :: Parser a -> Parser (Internal a)
internalParser p =
  choice
    [ setting
        [ switch ShowVersion,
          long "version",
          hidden
        ],
      setting
        [ switch RenderMan,
          long "render-man-page",
          hidden,
          help "Render a manpage"
        ],
      setting
        [ switch RenderDocumentation,
          long "render-reference-documentation",
          hidden,
          help "Render reference documentation"
        ],
      setting
        [ switch RenderNixosOptions,
          long "render-nix-options",
          hidden,
          help "Render Nix options"
        ],
      BashCompletionScript
        <$> setting
          [ option,
            reader $ maybeReader parseAbsFile,
            long "bash-completion-script",
            hidden,
            help "Render the bash completion script"
          ],
      ZshCompletionScript
        <$> setting
          [ option,
            reader $ maybeReader parseAbsFile,
            long "zsh-completion-script",
            hidden,
            help "Render the zsh completion script"
          ],
      ZshCompletionScript
        <$> setting
          [ option,
            reader $ maybeReader parseAbsFile,
            long "fish-completion-script",
            hidden,
            help "Render the fish completion script"
          ],
      setting
        [ help "Query completion",
          switch CompletionQuery,
          -- Long string that no normal user would ever use.
          long "query-opt-env-conf-completion",
          hidden
        ]
        <*> setting
          [ switch True,
            long "completion-enriched",
            value False,
            hidden,
            help "Whether to enable enriched completion"
          ]
        <*> setting
          [ option,
            reader auto,
            long "completion-index",
            hidden,
            help "The index between the arguments where completion was invoked."
          ]
        <*> many
          ( setting
              [ option,
                reader str,
                long "completion-word",
                hidden,
                help "The words (arguments) that have already been typed"
              ]
          ),
      ParsedNormally <$> p
    ]
