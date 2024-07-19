{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OptEnvConf.Run
  ( runSettingsParser,
    runParser,
    runParserOn,
    internalParser,
  )
where

import Autodocodec
import Control.Arrow (left)
import Control.Monad
import Control.Monad.Reader hiding (Reader, reader, runReader)
import Control.Monad.State
import Data.Aeson (parseJSON, (.:?))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as JSON
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Traversable
import Data.Version
import GHC.Stack (SrcLoc)
import OptEnvConf.Args as Args
import OptEnvConf.Completion
import OptEnvConf.Doc
import OptEnvConf.EnvMap (EnvMap (..))
import qualified OptEnvConf.EnvMap as EnvMap
import OptEnvConf.Error
import OptEnvConf.Lint
import OptEnvConf.NonDet
import OptEnvConf.Output
import OptEnvConf.Parser
import OptEnvConf.Reader
import OptEnvConf.Setting
import OptEnvConf.Validation
import Path
import System.Environment (getArgs, getEnvironment, getProgName)
import System.Exit
import System.IO
import Text.Colour
import Text.Colour.Capabilities.FromEnv

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
  allArgs <- getArgs
  let argMap' = parseArgs allArgs
  let mArgMap = consumeSwitch ["--debug-optparse"] argMap'
  let (debugMode, argMap) = case mArgMap of
        Nothing -> (False, argMap')
        Just am -> (True, am)

  completeEnv <- getEnvironment
  let envVars = EnvMap.parse completeEnv

  case lintParser p of
    Just errs -> do
      tc <- getTerminalCapabilitiesFromHandle stderr
      hPutChunksLocaleWith tc stderr $ renderLintErrors errs
      exitFailure
    Nothing -> do
      let p' = internalParser version p
      let docs = parserDocs p'
      mDebugMode <-
        if debugMode
          then Just <$> getTerminalCapabilitiesFromHandle stderr
          else pure Nothing
      errOrResult <-
        runParserOn
          mDebugMode
          p'
          argMap
          envVars
          Nothing
      case errOrResult of
        Left errs -> do
          tc <- getTerminalCapabilitiesFromHandle stderr
          hPutChunksLocaleWith tc stderr $ renderErrors errs
          exitFailure
        Right i -> case i of
          ShowHelp -> do
            progname <- getProgName
            tc <- getTerminalCapabilitiesFromHandle stdout
            hPutChunksLocaleWith tc stdout $ renderHelpPage progname progDesc docs
            exitSuccess
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
          CheckSettings -> do
            let argMap'' = case consumeSwitch [DashedLong settingsCheckSwitch] argMap of
                  Nothing -> error "If you see this there is a bug in opt-env-conf."
                  Just am -> am
            stderrTc <- getTerminalCapabilitiesFromHandle stderr
            errOrSets <- runParserOn (Just stderrTc) p argMap'' envVars Nothing
            case errOrSets of
              Left errs -> do
                hPutChunksLocaleWith stderrTc stderr $ renderErrors errs
                exitFailure
              Right _ -> do
                tc <- getTerminalCapabilitiesFromHandle stdout
                hPutChunksLocaleWith tc stdout ["Settings parsed successfully."]
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

-- Internal structure to help us do what the framework
-- is supposed to.
data Internal a
  = ShowHelp
  | ShowVersion
  | RenderMan
  | CheckSettings
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

settingsCheckSwitch :: NonEmpty Char
settingsCheckSwitch =
  -- Pretty long so it probably doesn't collide.
  'r' :| "un-settings-check"

internalParser :: Version -> Parser a -> Parser (Internal a)
internalParser version p =
  let allowLeftovers :: Parser a -> Parser a
      allowLeftovers p' = fst <$> ((,) <$> p' <*> many (setting [reader str, argument, hidden] :: Parser String))
   in choice
        [ allowLeftovers $
            setting
              [ switch ShowHelp,
                short 'h',
                long "help",
                help "Show this help text"
              ],
          allowLeftovers $
            setting
              [ switch ShowVersion,
                long "version",
                help $ "Output version information: " <> showVersion version
              ],
          setting
            [ switch RenderMan,
              long "render-man-page",
              hidden,
              help "Show this help text"
            ],
          allowLeftovers $
            setting
              [ switch CheckSettings,
                long $ NE.toList settingsCheckSwitch,
                hidden,
                help "Run the parser and exit if parsing succeeded."
              ],
          BashCompletionScript
            <$> mapIO
              parseAbsFile
              ( setting
                  [ option,
                    reader str,
                    long "bash-completion-script",
                    hidden,
                    help "Render the bash completion script"
                  ]
              ),
          ZshCompletionScript
            <$> mapIO
              parseAbsFile
              ( setting
                  [ option,
                    reader str,
                    long "zsh-completion-script",
                    hidden,
                    help "Render the zsh completion script"
                  ]
              ),
          ZshCompletionScript
            <$> mapIO
              parseAbsFile
              ( setting
                  [ option,
                    reader str,
                    long "fish-completion-script",
                    hidden,
                    help "Render the fish completion script"
                  ]
              ),
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

-- | Run a parser on given arguments and environment instead of getting them
-- from the current process.
runParserOn ::
  -- DebugMode
  Maybe TerminalCapabilities ->
  Parser a ->
  Args ->
  EnvMap ->
  Maybe JSON.Object ->
  IO (Either (NonEmpty ParseError) a)
runParserOn debugMode parser args envVars mConfig = do
  let ppState =
        PPState
          { ppStateArgs = args,
            ppStateParsedSettings = S.empty
          }
  let ppEnv =
        PPEnv
          { ppEnvEnv = envVars,
            ppEnvConf = mConfig,
            ppEnvDebug = debugMode,
            ppEnvIndent = 0
          }
  let go' = do
        result <- go parser
        leftoverArgs <- gets ppStateArgs
        case recogniseLeftovers leftoverArgs of
          Nothing -> pure result
          Just leftovers -> ppError Nothing $ ParseErrorUnrecognised leftovers
  mTup <- runPPLazy go' ppState ppEnv
  case mTup of
    Nothing -> error "TODO figure out when this list can be empty"
    Just ((errOrRes, _), nexts) -> case errOrRes of
      Success a -> pure (Right a)
      Failure firstErrors ->
        let goNexts ns = do
              -- TODO: Consider keeping around all errors?
              mNext <- runNonDetTLazy ns
              case mNext of
                Nothing -> pure $ Left $ (if isJust debugMode then id else eraseErrorSrcLocs) firstErrors
                Just ((eOR, _), ns') -> case eOR of
                  Success a -> pure (Right a)
                  Failure _ -> goNexts ns'
         in goNexts nexts
  where
    go ::
      Parser a ->
      PP a
    go = \case
      ParserPure a -> do
        debug [syntaxChunk "pure value"]
        pure a
      ParserAp ff fa -> do
        debug [syntaxChunk "Ap"]
        ppIndent $ go ff <*> go fa
      ParserEmpty mLoc -> do
        debug [syntaxChunk "Empty", ": ", mSrcLocChunk mLoc]
        ppError mLoc ParseErrorEmpty
      ParserSelect fe ff -> do
        debug [syntaxChunk "Select"]
        ppIndent $ select (go fe) (go ff)
      ParserAlt p1 p2 -> do
        debug [syntaxChunk "Alt"]
        ppIndent $ do
          debug ["Trying left side."]
          eor <- ppIndent $ tryPP (go p1)
          case eor of
            Just a -> do
              debug ["Left side succeeded."]
              pure a
            Nothing -> do
              debug ["Left side failed, trying right side."]
              ppIndent $ go p2
      ParserMany p' -> do
        debug [syntaxChunk "Many"]
        ppIndent $ do
          eor <- tryPP $ go p'
          case eor of
            Nothing -> pure []
            Just a -> do
              as <- go (ParserMany p')
              pure (a : as)
      ParserAllOrNothing mLoc p' -> do
        debug [syntaxChunk "AllOrNothing", ": ", mSrcLocChunk mLoc]
        ppIndent $ do
          e <- ask
          s <- get
          results <- liftIO $ runPP (go p') s e
          (result, s') <- ppNonDetList results
          put s'
          case result of
            Success a -> pure a
            Failure errs -> do
              if not $ all errorIsForgivable errs
                then ppErrors' errs
                else do
                  -- Settings available below
                  let settingsSet = parserSettingsSet p'
                  -- Settings that have been parsed
                  parsedSet <- gets ppStateParsedSettings
                  -- Settings that have been parsed below
                  let parsedSettingsSet = settingsSet `S.intersection` parsedSet
                  -- If any settings have been parsed below, and parsing still failed
                  -- (this is the case because we're in the failure branch)
                  -- with only forgivable errors
                  -- (this is the case because we're in the branch where that's been checked)
                  -- then this should be an unforgivable error.
                  if not (null parsedSettingsSet)
                    then ppErrors' $ errs <> (ParseError mLoc ParseErrorAllOrNothing :| [])
                    else ppErrors' errs
      ParserCheck mLoc forgivable f p' -> do
        debug [syntaxChunk "Parser with check", ": ", mSrcLocChunk mLoc]
        ppIndent $ do
          debug ["parser"]
          a <- ppIndent $ go p'
          debug ["check"]
          ppIndent $ do
            errOrB <- liftIO $ f a
            case errOrB of
              Left err -> do
                debug ["failed, forgivable: ", chunk $ T.pack $ show forgivable]
                ppError mLoc $ ParseErrorCheckFailed forgivable err
              Right b -> do
                debug ["succeeded"]
                pure b
      ParserCommands mLoc cs -> do
        debug [syntaxChunk "Commands", ": ", mSrcLocChunk mLoc]
        ppIndent $ do
          mS <- ppArg
          case mS of
            Nothing -> do
              debug ["No argument found for choosing a command."]
              ppError mLoc $ ParseErrorMissingCommand $ map commandArg cs
            Just s -> do
              case find ((== s) . commandArg) cs of
                Nothing -> do
                  debug ["Argument found, but no matching command: ", chunk $ T.pack $ show s]
                  ppError mLoc $ ParseErrorUnrecognisedCommand s (map commandArg cs)
                Just c -> do
                  debug ["Set command to ", commandChunk (commandArg c)]
                  go $ commandParser c
      ParserWithConfig pc pa -> do
        debug [syntaxChunk "WithConfig"]
        ppIndent $ do
          debug ["loading config"]
          mNewConfig <- ppIndent $ go pc
          debug ["with loaded config"]
          ppIndent $
            local (\e -> e {ppEnvConf = mNewConfig}) $
              go pa
      ParserSetting mLoc set@Setting {..} -> do
        debug [syntaxChunk "Setting", ": ", mSrcLocChunk mLoc]
        ppIndent $ do
          let markParsed = do
                maybe
                  (pure ())
                  ( \loc -> modify' $ \s ->
                      s
                        { ppStateParsedSettings =
                            S.insert
                              (hashSrcLoc loc)
                              (ppStateParsedSettings s)
                        }
                  )
                  mLoc
          let mOptDoc = settingOptDoc set
          mArg <-
            if settingTryArgument
              then do
                -- Require readers before finding the argument so the parser
                -- always fails if it's missing a reader.
                rs <- requireReaders settingReaders
                mS <- ppArg
                case mS of
                  Nothing -> do
                    debug ["could not set based on argument: no argument"]
                    pure NotFound
                  Just argStr -> do
                    case tryReaders rs argStr of
                      Left errs -> ppError mLoc $ ParseErrorArgumentRead mOptDoc errs
                      Right a -> do
                        debug
                          [ "set based on argument: ",
                            chunk $ T.pack $ show argStr
                          ]
                        pure $ Found a
              else pure NotRun

          case mArg of
            Found a -> do
              markParsed
              pure a
            _ -> do
              -- TODO do this without all the nesting
              mSwitch <- case settingSwitchValue of
                Nothing -> pure NotRun
                Just a -> do
                  mS <- ppSwitch settingDasheds
                  case mS of
                    Nothing -> do
                      debug
                        [ "could not set based on switch, no switch: ",
                          chunk $ T.pack $ show $ map renderDashed settingDasheds
                        ]
                      pure NotFound
                    Just () -> do
                      debug ["set based on switch."]
                      pure $ Found a

              case mSwitch of
                Found a -> do
                  markParsed
                  pure a
                _ -> do
                  mOpt <-
                    if settingTryOption
                      then do
                        -- Require readers before finding the option so the parser
                        -- always fails if it's missing a reader.
                        rs <- requireReaders settingReaders
                        mS <- ppOpt settingDasheds
                        case mS of
                          Nothing -> do
                            debug
                              [ "could not set based on options, no option: ",
                                chunk $ T.pack $ show $ map renderDashed settingDasheds
                              ]
                            pure NotFound
                          Just optionStr -> do
                            case tryReaders rs optionStr of
                              Left err -> ppError mLoc $ ParseErrorOptionRead mOptDoc err
                              Right a -> do
                                debug
                                  [ "set based on option: ",
                                    chunk $ T.pack $ show optionStr
                                  ]
                                pure $ Found a
                      else pure NotRun

                  case mOpt of
                    Found a -> do
                      markParsed
                      pure a
                    _ -> do
                      let mEnvDoc = settingEnvDoc set
                      mEnv <- case settingEnvVars of
                        Nothing -> pure NotRun
                        Just ne -> do
                          -- Require readers before finding the env vars so the parser
                          -- always fails if it's missing a reader.
                          rs <- requireReaders settingReaders
                          es <- asks ppEnvEnv
                          let founds = mapMaybe (`EnvMap.lookup` es) (NE.toList ne)
                          -- Run the parser on all specified env vars before
                          -- returning the first because we want to fail if any
                          -- of them fail, even if they wouldn't be the parse
                          -- result.
                          results <- for founds $ \varStr ->
                            case tryReaders rs varStr of
                              Left errs -> ppError mLoc $ ParseErrorEnvRead mEnvDoc errs
                              Right a -> do
                                debug
                                  [ "set based on env: ",
                                    chunk $ T.pack $ show varStr
                                  ]
                                pure a
                          case listToMaybe results of
                            Nothing -> do
                              debug
                                [ "could not set based on env vars, no var: ",
                                  chunk $ T.pack $ show $ maybe [] NE.toList settingEnvVars
                                ]
                              pure NotFound
                            Just a -> pure $ Found a

                      case mEnv of
                        Found a -> do
                          markParsed
                          pure a
                        _ -> do
                          let mConfDoc = settingConfDoc set
                          mConf <- case settingConfigVals of
                            Nothing -> pure NotRun
                            Just ((ne, DecodingCodec c) :| _) -> do
                              -- TODO try parsing with the others
                              mObj <- asks ppEnvConf
                              case mObj of
                                Nothing -> do
                                  debug ["no config object to set from"]
                                  pure NotFound
                                Just obj -> do
                                  let jsonParser :: JSON.Object -> NonEmpty String -> JSON.Parser (Maybe JSON.Value)
                                      jsonParser o (k :| rest) = case NE.nonEmpty rest of
                                        Nothing -> do
                                          case KeyMap.lookup (Key.fromString k) o of
                                            Nothing -> pure Nothing
                                            Just v -> Just <$> parseJSON v
                                        Just neRest -> do
                                          mO' <- o .:? Key.fromString k
                                          case mO' of
                                            Nothing -> pure Nothing
                                            Just o' -> jsonParser o' neRest
                                  case JSON.parseEither (jsonParser obj) ne of
                                    Left err -> ppError mLoc $ ParseErrorConfigRead mConfDoc err
                                    Right mV -> case mV of
                                      Nothing -> do
                                        debug
                                          [ "could not set based on config value, not configured: ",
                                            chunk $ T.pack $ show $ NE.toList ne
                                          ]
                                        pure NotFound
                                      Just v -> case JSON.parseEither (parseJSONVia c) v of
                                        Left err -> ppError mLoc $ ParseErrorConfigRead mConfDoc err
                                        Right mA -> case mA of
                                          Nothing -> do
                                            debug
                                              [ "could not set based on config value, configured to nothing: ",
                                                chunk $ T.pack $ show $ NE.toList ne
                                              ]
                                            pure NotFound
                                          Just a -> do
                                            debug
                                              [ "set based on config value:",
                                                chunk $ T.pack $ show v
                                              ]
                                            pure $ Found a

                          case mConf of
                            Found a -> do
                              markParsed
                              pure a
                            _ ->
                              case settingDefaultValue of
                                Just (a, _) -> do
                                  debug ["set to default value"]
                                  pure a -- Don't mark as parsed
                                Nothing -> do
                                  let parseResultError e res = case res of
                                        NotRun -> Nothing
                                        NotFound -> Just e
                                        Found _ -> Nothing -- Should not happen.
                                  debug ["not found"]
                                  maybe (ppError mLoc ParseErrorEmptySetting) (ppErrors mLoc) $
                                    NE.nonEmpty $
                                      catMaybes
                                        [ parseResultError (ParseErrorMissingArgument mOptDoc) mArg,
                                          parseResultError (ParseErrorMissingSwitch mOptDoc) mSwitch,
                                          parseResultError (ParseErrorMissingOption mOptDoc) mOpt,
                                          parseResultError (ParseErrorMissingEnvVar mEnvDoc) mEnv,
                                          parseResultError (ParseErrorMissingConfVal mConfDoc) mConf
                                        ]

data ParseResult a
  = NotRun
  | NotFound
  | Found a

requireReaders :: [Reader a] -> PP (NonEmpty (Reader a))
requireReaders rs = case NE.nonEmpty rs of
  Nothing -> ppError Nothing ParseErrorNoReaders
  Just ne -> pure ne

-- Try the readers in order
tryReaders :: NonEmpty (Reader a) -> String -> Either (NonEmpty String) a
tryReaders rs s = left NE.reverse $ go rs
  where
    go (r :| rl) = case runReader r s of
      Left err -> go' (err :| []) rl
      Right a -> Right a
    go' errs = \case
      [] -> Left errs
      (r : rl) -> case runReader r s of
        Left err -> go' (err <| errs) rl
        Right a -> Right a

type PP a = ReaderT PPEnv (ValidationT ParseError (StateT PPState (NonDetT IO))) a

runPP ::
  PP a ->
  PPState ->
  PPEnv ->
  IO [(Validation ParseError a, PPState)]
runPP p args envVars =
  runNonDetT (runStateT (runValidationT (runReaderT p envVars)) args)

runPPLazy ::
  PP a ->
  PPState ->
  PPEnv ->
  IO
    ( Maybe
        ( (Validation ParseError a, PPState),
          NonDetT IO (Validation ParseError a, PPState)
        )
    )
runPPLazy p args envVars =
  runNonDetTLazy (runStateT (runValidationT (runReaderT p envVars)) args)

tryPP :: PP a -> PP (Maybe a)
tryPP pp = do
  s <- get
  e <- ask
  results <- liftIO $ runPP pp s e
  (errOrRes, s') <- ppNonDetList results
  case errOrRes of
    Failure errs ->
      if all errorIsForgivable errs
        then do
          pure Nothing
        else ppErrors' errs
    Success a -> do
      put s' -- Only set state if parsing succeeded.
      pure $ Just a

ppNonDet :: NonDetT IO a -> PP a
ppNonDet = lift . lift . lift

ppNonDetList :: [a] -> PP a
ppNonDetList = ppNonDet . liftNonDetTList

data PPState = PPState
  { ppStateArgs :: !Args,
    ppStateParsedSettings :: !(Set SrcLocHash)
  }

data PPEnv = PPEnv
  { ppEnvEnv :: !EnvMap,
    ppEnvConf :: !(Maybe JSON.Object),
    -- Nothing means "not debug mode"
    ppEnvDebug :: !(Maybe TerminalCapabilities),
    ppEnvIndent :: !Int
  }

debug :: [Chunk] -> PP ()
debug chunks = do
  debugMode <- asks ppEnvDebug
  forM_ debugMode $ \tc -> do
    i <- asks ppEnvIndent
    -- Debug mode needs to involve an impure print because parsers can run IO
    -- actions and we need to see their output interleaved with the debug
    -- output
    liftIO $
      hPutChunksLocaleWith tc stderr $
        (replicate i "  " ++ chunks)
          ++ [ "\n"
             ]

ppIndent :: PP a -> PP a
ppIndent =
  local
    (\e -> e {ppEnvIndent = succ (ppEnvIndent e)})

ppArg :: PP (Maybe String)
ppArg = do
  args <- gets ppStateArgs
  debug ["Trying to consume an argument"]
  let consumePossibilities = Args.consumeArgument args
  if null consumePossibilities
    then debug ["Found no consume possibilities."]
    else do
      debug ["Found these possibilities to consume an argument:"]
      forM_ consumePossibilities $ \p ->
        debug [chunk $ T.pack $ show p]
  p@(mA, args') <- ppNonDetList consumePossibilities
  debug ["Considering this posibility: ", chunk $ T.pack $ show p]
  modify' (\s -> s {ppStateArgs = args'})
  pure mA

ppOpt :: [Dashed] -> PP (Maybe String)
ppOpt ds = do
  args <- gets ppStateArgs
  case Args.consumeOption ds args of
    Nothing -> pure Nothing
    Just (a, args') -> do
      modify' (\s -> s {ppStateArgs = args'})
      pure (Just a)

ppSwitch :: [Dashed] -> PP (Maybe ())
ppSwitch ds = do
  args <- gets ppStateArgs
  case Args.consumeSwitch ds args of
    Nothing -> pure Nothing
    Just args' -> do
      modify' (\s -> s {ppStateArgs = args'})
      pure (Just ())

ppErrors' :: NonEmpty ParseError -> PP a
ppErrors' = lift . ValidationT . lift . pure . Failure

ppErrors :: Maybe SrcLoc -> NonEmpty ParseErrorMessage -> PP a
ppErrors mLoc = ppErrors' . NE.map (ParseError mLoc)

ppError :: Maybe SrcLoc -> ParseErrorMessage -> PP a
ppError mLoc = ppErrors mLoc . NE.singleton
