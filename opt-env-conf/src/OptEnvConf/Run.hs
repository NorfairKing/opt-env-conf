{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OptEnvConf.Run where

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
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Traversable
import GHC.Stack (SrcLoc)
import OptEnvConf.Args as Args
import OptEnvConf.Doc
import OptEnvConf.EnvMap (EnvMap (..))
import qualified OptEnvConf.EnvMap as EnvMap
import OptEnvConf.Error
import OptEnvConf.NonDet
import OptEnvConf.Output
import OptEnvConf.Parser
import OptEnvConf.Reader
import OptEnvConf.Setting
import OptEnvConf.Validation
import System.IO
import Text.Colour

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
runParserOn mDebugMode parser args envVars mConfig = do
  let ppState = mkPPState args
  let ppEnv = mkPPEnv envVars mConfig mDebugMode
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
                Nothing ->
                  pure $
                    Left $
                      let f = case mDebugMode of
                            Nothing -> eraseErrorSrcLocs
                            Just _ -> id
                       in f firstErrors
                Just ((eOR, _), ns') -> case eOR of
                  Success a -> pure (Right a)
                  Failure _ -> goNexts ns'
         in goNexts nexts
  where
    go ::
      Parser a ->
      PP a
    go = \case
      ParserPure a -> ppPure a
      ParserAp ff fa -> do
        debug [syntaxChunk "Ap"]
        ppIndent $ go ff <*> go fa
      ParserEmpty mLoc -> ppEmpty mLoc
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
      ParserMany mLoc p' -> do
        debug [syntaxChunk "Many", ": ", mSrcLocChunk mLoc]
        ppIndent $ do
          eor <- tryPP $ go p'
          case eor of
            Nothing -> pure []
            Just a -> do
              as <- go (ParserMany mLoc p')
              pure (a : as)
      ParserSome mLoc p' -> do
        debug [syntaxChunk "Some", ": ", mSrcLocChunk mLoc]
        ppIndent $ do
          a <- go p'
          debug ["First element of some succeeded, continuing with Many"]
          as <- go (ParserMany mLoc p')
          pure (a :| as)
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
                  let settingsMap = parserSettingsMap p'
                  -- Settings that have been parsed
                  parsedMap <- gets ppStateParsedSettings
                  -- Settings that have been parsed below
                  let parsedSettingsMap = settingsMap `M.intersection` parsedMap
                  -- If any settings have been parsed below, and parsing still failed
                  -- (this is the case because we're in the failure branch)
                  -- with only forgivable errors
                  -- (this is the case because we're in the branch where that's been checked)
                  -- then this should be an unforgivable error.
                  if null parsedSettingsMap
                    then ppErrors' errs
                    else ppErrors' $ errs <> (ParseError mLoc (ParseErrorAllOrNothing parsedSettingsMap) :| [])
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
      ParserCommands mLoc mDefault cs -> do
        debug [syntaxChunk "Commands", ": ", mSrcLocChunk mLoc]
        forM_ mDefault $ \d -> debug ["default:", chunk $ T.pack $ show d]
        ppIndent $ do
          mS <- ppArg
          let docsForErrors = map (void . commandParserDocs) cs
          case mS of
            Nothing -> do
              debug ["No argument found for choosing a command."]
              let mDefaultCommand = do
                    d <- mDefault
                    find ((== d) . commandArg) cs
              case mDefaultCommand of
                Nothing -> ppError mLoc $ ParseErrorMissingCommand docsForErrors
                Just dc -> do
                  debug ["Choosing default command: ", commandChunk (commandArg dc)]
                  go $ commandParser dc
            Just s -> do
              case find ((== s) . commandArg) cs of
                Nothing -> ppError mLoc $ ParseErrorUnrecognisedCommand s docsForErrors
                Just c -> do
                  debug ["Set command to ", commandChunk (commandArg c)]
                  go $ commandParser c
      ParserWithConfig mLoc pc pa -> do
        debug [syntaxChunk "WithConfig", ": ", mSrcLocChunk mLoc]
        ppIndent $ do
          debug ["loading config"]
          mNewConfig <- ppIndent $ go pc
          debug ["with loaded config"]
          ppIndent $
            local (\e -> e {ppEnvConf = mNewConfig}) $
              go pa
      ParserSetting mLoc set -> ppSetting mLoc set

ppPure :: a -> PP a
ppPure a = do
  debug [syntaxChunk "pure value"]
  pure a

ppEmpty :: Maybe SrcLoc -> PP a
ppEmpty mLoc = do
  debug [syntaxChunk "Empty", ": ", mSrcLocChunk mLoc]
  ppError mLoc ParseErrorEmpty

ppSetting ::
  Maybe SrcLoc ->
  Setting a ->
  PP a
ppSetting mLoc set@Setting {..} = do
  debug [syntaxChunk "Setting", ": ", mSrcLocChunk mLoc]
  ppIndent $ do
    let markParsed = do
          maybe
            (pure ())
            ( \loc -> modify' $ \s ->
                s
                  { ppStateParsedSettings =
                      M.insert
                        (hashSetting set)
                        loc
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
                    let founds = mapMaybe ((`EnvMap.lookup` es) . envVarSettingVar) (NE.toList ne)
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
                      Just confSets -> do
                        mObj <- asks ppEnvConf
                        case mObj of
                          Nothing -> do
                            debug ["no config object to set from"]
                            pure NotFound
                          Just obj -> do
                            let goConfSet ConfigValSetting {..} = do
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
                                  case JSON.parseEither (jsonParser obj) configValSettingPath of
                                    Left err -> ppError mLoc $ ParseErrorConfigRead mConfDoc err
                                    Right mV -> case mV of
                                      Nothing -> do
                                        debug
                                          [ "could not set based on config value, not configured: ",
                                            chunk $ T.pack $ show $ NE.toList configValSettingPath
                                          ]
                                        pure Nothing
                                      Just v -> case JSON.parseEither (parseJSONVia configValSettingCodec) v of
                                        Left err -> ppError mLoc $ ParseErrorConfigRead mConfDoc err
                                        Right mA -> case mA of
                                          Nothing -> do
                                            debug
                                              [ "could not set based on config value, configured to nothing: ",
                                                chunk $ T.pack $ show $ NE.toList configValSettingPath
                                              ]
                                            pure Nothing
                                          Just a -> do
                                            debug
                                              [ "set based on config value: ",
                                                chunk $ T.pack $ show v
                                              ]
                                            pure $ Just a
                            let toRes = \case
                                  Nothing -> NotFound
                                  Just a -> Found a
                            let goConfSets (confSet :| rest) = case NE.nonEmpty rest of
                                  Nothing -> toRes <$> goConfSet confSet
                                  Just ne -> do
                                    res <- goConfSet confSet
                                    case res of
                                      Just a -> pure $ Found a
                                      Nothing -> goConfSets ne
                            goConfSets confSets
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

runHelpParser ::
  -- DebugMode
  Maybe TerminalCapabilities ->
  Args ->
  Parser a ->
  IO (Either (NonEmpty ParseError) (Maybe ([String], CommandDoc (Maybe SetDoc))))
runHelpParser mDebugMode args parser = do
  let ppState = mkPPState args
  let ppEnv = mkPPEnv EnvMap.empty Nothing mDebugMode
  mResOrNext <- runPPLazy (go' [] parser) ppState ppEnv
  case mResOrNext of
    Nothing -> pure $ Right Nothing
    Just ((result, _), _) -> pure $ case result of
      Failure errs -> Left errs
      Success mDocs -> Right mDocs
  where
    -- We try to parse the commands as deep as possible and ignore everything else.
    go' :: [String] -> Parser a -> PP (Maybe ([String], CommandDoc (Maybe SetDoc)))
    go' path =
      let go :: Parser a -> PP (Maybe ([String], CommandDoc (Maybe SetDoc)))
          go = go' path
       in \case
            ParserPure _ -> do
              debug [syntaxChunk "pure value"]
              pure Nothing
            ParserAp ff fa -> do
              debug [syntaxChunk "Ap"]
              ppIndent $ do
                mf <- go ff
                ma <- go fa
                pure $ ma <|> mf -- Reverse order
            ParserSelect fe ff -> do
              debug [syntaxChunk "Select"]
              ppIndent $ do
                me <- go fe
                mf <- go ff
                pure $ mf <|> me -- Reverse order
            ParserEmpty mLoc -> do
              debug [syntaxChunk "Empty", ": ", mSrcLocChunk mLoc]
              pure Nothing
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
            ParserMany mLoc p' -> do
              debug [syntaxChunk "Many", ": ", mSrcLocChunk mLoc]
              ppIndent $ go p'
            ParserSome mLoc p' -> do
              debug [syntaxChunk "Some", ": ", mSrcLocChunk mLoc]
              ppIndent $ go p'
            ParserAllOrNothing mLoc p' -> do
              debug [syntaxChunk "AllOrNothing", ": ", mSrcLocChunk mLoc]
              ppIndent $ go p'
            ParserCheck mLoc _ _ p' -> do
              debug [syntaxChunk "Parser with check", ": ", mSrcLocChunk mLoc]
              ppIndent $ go p'
            ParserWithConfig mLoc pc pa -> do
              debug [syntaxChunk "WithConfig", ": ", mSrcLocChunk mLoc]
              ppIndent $ do
                mNewConfig <- go pc
                mRes <- go pa
                pure $ mRes <|> mNewConfig -- Reverse order
            ParserSetting mLoc _ -> do
              debug [syntaxChunk "Setting", ": ", mSrcLocChunk mLoc]
              pure Nothing
            ParserCommands mLoc mDefault cs -> do
              debug [syntaxChunk "Commands", ": ", mSrcLocChunk mLoc]
              forM_ mDefault $ \d -> debug ["default:", chunk $ T.pack $ show d]
              ppIndent $ do
                mS <- ppArg
                case mS of
                  Nothing -> do
                    debug ["No argument found for choosing a command."]
                    pure Nothing
                  Just s -> do
                    case find ((== s) . commandArg) cs of
                      Nothing -> do
                        debug ["Argument found, but no matching command: ", chunk $ T.pack $ show s]
                        pure Nothing
                      Just c -> do
                        debug ["Set command to ", commandChunk (commandArg c)]
                        mRes <- go' (commandArg c : path) $ commandParser c
                        pure $ case mRes of
                          Nothing -> Just (reverse path, commandParserDocs c)
                          Just res -> pure res

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
    ppStateParsedSettings :: !(Map SettingHash SrcLoc)
  }

mkPPState :: Args -> PPState
mkPPState args =
  PPState
    { ppStateArgs = args,
      ppStateParsedSettings = M.empty
    }

data PPEnv = PPEnv
  { ppEnvEnv :: !EnvMap,
    ppEnvConf :: !(Maybe JSON.Object),
    -- Nothing means "not debug mode"
    ppEnvDebug :: !(Maybe TerminalCapabilities),
    ppEnvIndent :: !Int
  }

mkPPEnv :: EnvMap -> Maybe JSON.Object -> Maybe TerminalCapabilities -> PPEnv
mkPPEnv envVars mConfig mDebugMode =
  PPEnv
    { ppEnvEnv = envVars,
      ppEnvConf = mConfig,
      ppEnvDebug = mDebugMode,
      ppEnvIndent = 0
    }

debug ::
  (MonadIO m, MonadReader PPEnv m) =>
  [Chunk] ->
  m ()
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

ppIndent ::
  (MonadReader PPEnv m) =>
  m a ->
  m a
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
