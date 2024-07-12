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
      errOrResult <-
        runParserOn
          p'
          argMap
          envVars
          Nothing
      case errOrResult of
        Left errs -> do
          tc <- getTerminalCapabilitiesFromHandle stderr
          let f = if debugMode then id else eraseErrorSrcLocs
          hPutChunksLocaleWith tc stderr $ renderErrors $ f errs
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

internalParser :: Version -> Parser a -> Parser (Internal a)
internalParser version p =
  choice
    [ setting
        [ switch ShowHelp,
          short 'h',
          long "help",
          help "Show this help text"
        ],
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
  Parser a ->
  Args ->
  EnvMap ->
  Maybe JSON.Object ->
  IO (Either (NonEmpty ParseError) a)
runParserOn parser args envVars mConfig = do
  let ppState =
        PPState
          { ppStateArgs = args,
            ppStateParsedSettings = S.empty
          }
  let ppEnv =
        PPEnv
          { ppEnvEnv = envVars,
            ppEnvConf = mConfig
          }
  let go' = do
        result <- go parser
        leftovers <- gets ppStateArgs
        if null (argsLeftovers leftovers)
          then pure result
          else ppError Nothing ParseErrorUnrecognised --
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
                Nothing -> pure (Left firstErrors)
                Just ((eOR, _), ns') -> case eOR of
                  Success a -> pure (Right a)
                  Failure _ -> goNexts ns'
         in goNexts nexts
  where
    go ::
      Parser a ->
      PP a
    go = \case
      ParserPure a -> pure a
      ParserAp ff fa -> go ff <*> go fa
      ParserEmpty mLoc -> ppError mLoc ParseErrorEmpty
      ParserSelect fe ff -> select (go fe) (go ff)
      ParserAlt p1 p2 -> do
        eor <- tryPP (go p1)
        case eor of
          Just a -> pure a
          Nothing -> go p2
      ParserMany p' -> do
        eor <- tryPP $ go p'
        case eor of
          Nothing -> pure []
          Just a -> do
            as <- go (ParserMany p')
            pure (a : as)
      ParserAllOrNothing mLoc p' -> do
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
        a <- go p'
        errOrB <- liftIO $ f a
        case errOrB of
          Left err -> ppError mLoc $ ParseErrorCheckFailed forgivable err
          Right b -> pure b
      ParserCommands mLoc cs -> do
        mS <- ppArg
        case mS of
          Nothing -> ppError mLoc $ ParseErrorMissingCommand $ map commandArg cs
          Just s -> case find ((== s) . commandArg) cs of
            Nothing -> ppError mLoc $ ParseErrorUnrecognisedCommand s (map commandArg cs)
            Just c -> go $ commandParser c
      ParserWithConfig pc pa -> do
        mNewConfig <- go pc
        local (\e -> e {ppEnvConf = mNewConfig}) $ go pa
      ParserSetting mLoc set@Setting {..} -> do
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
                Nothing -> pure NotFound
                Just argStr -> do
                  case tryReaders rs argStr of
                    Left errs -> ppError mLoc $ ParseErrorArgumentRead mOptDoc errs
                    Right a -> pure $ Found a
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
                  Nothing -> pure NotFound
                  Just () -> pure $ Found a

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
                        Nothing -> pure NotFound
                        Just optionStr -> do
                          case tryReaders rs optionStr of
                            Left err -> ppError mLoc $ ParseErrorOptionRead mOptDoc err
                            Right a -> pure $ Found a
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
                            Right a -> pure a
                        pure $ maybe NotFound Found $ listToMaybe results

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
                              Nothing -> pure NotFound
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
                                    Nothing -> pure NotFound
                                    Just v -> case JSON.parseEither (parseJSONVia c) v of
                                      Left err -> ppError mLoc $ ParseErrorConfigRead mConfDoc err
                                      Right a -> pure $ maybe NotFound Found a

                        case mConf of
                          Found a -> do
                            markParsed
                            pure a
                          _ ->
                            case settingDefaultValue of
                              Just (a, _) -> pure a -- Don't mark as parsed
                              Nothing -> do
                                let parseResultError e res = case res of
                                      NotRun -> Nothing
                                      NotFound -> Just e
                                      Found _ -> Nothing -- Should not happen.
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
  put s'
  case errOrRes of
    Failure errs ->
      if all errorIsForgivable errs
        then pure Nothing
        else ppErrors' errs
    Success a -> pure $ Just a

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
    ppEnvConf :: !(Maybe JSON.Object)
  }

ppArg :: PP (Maybe String)
ppArg = do
  args <- gets ppStateArgs
  let consumePossibilities = Args.consumeArgument args
  (mA, args') <- ppNonDetList consumePossibilities
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
