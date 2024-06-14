{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module OptEnvConf.Run
  ( runSettingsParser,
    runParser,
    runParserComplete,
    runParserOn,
    internalParser,
    unrecognisedOptions,
  )
where

import Autodocodec
import Control.Arrow (left)
import Control.Monad
import Control.Monad.Reader hiding (Reader)
import Control.Monad.State
import Data.Aeson ((.:?))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as JSON
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Version
import OptEnvConf.ArgMap (ArgMap (..), Dashed (..), Opt (..))
import qualified OptEnvConf.ArgMap as ArgMap
import OptEnvConf.Doc
import OptEnvConf.EnvMap (EnvMap (..))
import qualified OptEnvConf.EnvMap as EnvMap
import OptEnvConf.Error
import OptEnvConf.Lint
import OptEnvConf.Parser
import OptEnvConf.Reader
import OptEnvConf.Setting
import OptEnvConf.Validation
import System.Environment (getArgs, getEnvironment, getProgName)
import System.Exit
import System.IO
import Text.Colour
import Text.Colour.Capabilities.FromEnv

runSettingsParser :: (HasParser a) => Version -> IO a
runSettingsParser version = runParser version settingsParser

runParser :: Version -> Parser a -> IO a
runParser version = fmap fst . runParserWithLeftovers version

runParserWithLeftovers :: Version -> Parser a -> IO (a, [String])
runParserWithLeftovers version p = do
  args <- getArgs
  let (argMap, leftovers) = ArgMap.parse args
  envVars <- EnvMap.parse <$> getEnvironment

  case lintParser p of
    Just errs -> do
      tc <- getTerminalCapabilitiesFromHandle stderr
      hPutChunksLocaleWith tc stderr $ renderLintErrors errs
      exitFailure
    Nothing -> do
      let p' = internalParser version p
      let docs = parserDocs p'
      errOrResult <-
        runParserComplete
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
            hPutChunksLocaleWith tc stdout $ renderHelpPage progname docs
            exitSuccess
          ShowVersion -> do
            progname <- getProgName
            tc <- getTerminalCapabilitiesFromHandle stdout
            hPutChunksLocaleWith tc stdout $ renderVersionPage progname version
            exitSuccess
          RenderMan -> do
            progname <- getProgName
            tc <- getTerminalCapabilitiesFromHandle stdout
            hPutChunksLocaleWith tc stdout $ renderManPage progname docs
            exitSuccess
          ParsedNormally a -> pure (a, leftovers)

-- Internal structure to help us do what the framework
-- is supposed to.
data Internal a
  = ShowHelp
  | ShowVersion
  | RenderMan
  | ParsedNormally a

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
          short 'v',
          long "version",
          help $ "Output version information: " <> showVersion version
        ],
      setting
        [ switch RenderMan,
          long "render-man-page",
          hidden,
          help "Show this help text"
        ],
      ParsedNormally <$> p
    ]

-- 'runParserOn' _and_ 'unrecognisedOptions'
runParserComplete ::
  Parser a ->
  ArgMap ->
  EnvMap ->
  Maybe JSON.Object ->
  IO (Either (NonEmpty ParseError) a)
runParserComplete p args e mConf =
  case NE.nonEmpty $ unrecognisedOptions p args of
    Just unrecogniseds -> pure $ Left $ NE.map ParseErrorUnrecognised unrecogniseds
    Nothing -> runParserOn p args e mConf

unrecognisedOptions :: Parser a -> ArgMap -> [Opt]
unrecognisedOptions p args =
  let possibleOpts = collectPossibleOpts p
      isRecognised =
        (`S.member` possibleOpts) . \case
          OptArg _ -> PossibleArg
          OptSwitch d -> PossibleSwitch d
          OptOption d _ -> PossibleOption d
   in filter (not . isRecognised) (argMapOpts args)

data PossibleOpt
  = PossibleArg
  | PossibleSwitch !Dashed
  | PossibleOption !Dashed
  deriving (Show, Eq, Ord)

collectPossibleOpts :: Parser a -> Set PossibleOpt
collectPossibleOpts = go
  where
    go :: Parser a -> Set PossibleOpt
    go = \case
      ParserPure _ -> S.empty
      ParserFmap _ p -> go p
      ParserAp p1 p2 -> go p1 `S.union` go p2
      ParserSelect p1 p2 -> go p1 `S.union` go p2
      ParserEmpty -> S.empty
      ParserAlt p1 p2 -> go p1 `S.union` go p2
      ParserMany p -> go p
      ParserMapIO _ p -> go p
      ParserWithConfig pc pa -> go pc `S.union` go pa
      ParserSetting Setting {..} ->
        S.fromList $
          concat
            [ [PossibleArg | settingTryArgument],
              case settingSwitchValue of
                Nothing -> []
                Just _ -> map PossibleSwitch settingDasheds,
              if settingTryOption
                then map PossibleOption settingDasheds
                else []
            ]

runParserOn ::
  Parser a ->
  ArgMap ->
  EnvMap ->
  Maybe JSON.Object ->
  IO (Either (NonEmpty ParseError) a)
runParserOn p args envVars mConfig =
  validationToEither <$> do
    let ppEnv = PPEnv {ppEnvEnv = envVars, ppEnvConf = mConfig}
    runValidationT $ evalStateT (runReaderT (go p) ppEnv) args
  where
    tryPP :: PP a -> PP (Either (NonEmpty ParseError) (a, PPState))
    tryPP pp = do
      s <- get
      e <- ask
      liftIO $ runPP pp s e
    go ::
      Parser a ->
      PP a
    go = \case
      ParserFmap f p' -> f <$> go p'
      ParserPure a -> pure a
      ParserAp ff fa -> go ff <*> go fa
      ParserEmpty -> ppError ParseErrorEmpty
      ParserSelect fe ff -> select (go fe) (go ff)
      ParserAlt p1 p2 -> do
        eor <- tryPP (go p1)
        case eor of
          Right (a, s') -> do
            put s'
            pure a
          -- Note that args are not consumed if the alternative failed.
          Left _ -> go p2 -- TODO: Maybe collect the error?
      ParserMany p' -> do
        eor <- tryPP $ go p'
        case eor of
          Left _ -> pure [] -- Err if fails, the end
          Right (a, s') -> do
            put s'
            as <- go (ParserMany p')
            pure (a : as)
      ParserMapIO f p' -> do
        a <- go p'
        liftIO $ f a
      ParserWithConfig pc pa -> do
        mNewConfig <- go pc
        local (\e -> e {ppEnvConf = mNewConfig}) $ go pa
      ParserSetting set@Setting {..} -> do
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
                    Left errs -> ppError $ ParseErrorArgumentRead errs
                    Right a -> pure $ Found a
            else pure NotRun

        case mArg of
          Found a -> pure a
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
              Found a -> pure a
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
                            Left err -> ppError $ ParseErrorOptionRead err
                            Right a -> pure $ Found a
                    else pure NotRun

                case mOpt of
                  Found a -> pure a
                  _ -> do
                    mEnv <- case settingEnvVars of
                      Nothing -> pure NotRun
                      Just ne -> do
                        -- Require readers before finding the env vars so the parser
                        -- always fails if it's missing a reader.
                        rs <- requireReaders settingReaders

                        let vars = NE.toList ne

                        es <- asks ppEnvEnv
                        case msum $ map (`EnvMap.lookup` es) vars of
                          Nothing -> pure NotFound
                          Just varStr ->
                            case tryReaders rs varStr of
                              Left errs -> ppError $ ParseErrorEnvRead errs
                              Right a -> pure $ Found a

                    case mEnv of
                      Found a -> pure a
                      _ -> do
                        mConf <- case settingConfigVals of
                          Nothing -> pure NotRun
                          Just ((ne, DecodingCodec c) :| _) -> do
                            -- TODO try parsing with the others
                            -- TODO handle subconfig prefix here?
                            mObj <- asks ppEnvConf
                            case mObj of
                              Nothing -> pure NotFound
                              Just obj -> do
                                let jsonParser :: JSON.Object -> NonEmpty String -> JSON.Parser (Maybe JSON.Value)
                                    jsonParser o (k :| rest) = case NE.nonEmpty rest of
                                      Nothing -> o .:? Key.fromString k
                                      Just neRest -> do
                                        mO' <- o .:? Key.fromString k
                                        case mO' of
                                          Nothing -> pure Nothing
                                          Just o' -> jsonParser o' neRest
                                case JSON.parseEither (jsonParser obj) ne of
                                  Left err -> ppError $ ParseErrorConfigRead err
                                  Right mV -> case mV of
                                    Nothing -> pure NotFound
                                    Just v -> case JSON.parseEither (parseJSONVia c) v of
                                      Left err -> ppError $ ParseErrorConfigRead err
                                      Right a -> pure $ Found a

                        case mConf of
                          Found a -> pure a
                          _ ->
                            case settingDefaultValue of
                              Just (a, _) -> pure a
                              Nothing -> do
                                let mOptDoc = settingOptDoc set
                                let mEnvDoc = settingEnvDoc set
                                let mConfDoc = settingConfDoc set
                                let parseResultError e res = case res of
                                      NotRun -> Nothing
                                      NotFound -> Just e
                                      Found _ -> Nothing -- Should not happen.
                                maybe (ppError ParseErrorEmptySetting) ppErrors $
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
  Nothing -> error "no readers configured." -- TODO nicer error
  Just ne -> pure ne

-- Try the readers in order
tryReaders :: NonEmpty (Reader a) -> String -> Either (NonEmpty String) a
tryReaders rs s = left NE.reverse $ go rs
  where
    go (r :| rl) = case r s of
      Left err -> go' (err :| []) rl
      Right a -> Right a
    go' errs = \case
      [] -> Left errs
      (r : rl) -> case r s of
        Left err -> go' (err <| errs) rl
        Right a -> Right a

type PP a = ReaderT PPEnv (StateT PPState (ValidationT ParseError IO)) a

type PPState = ArgMap

data PPEnv = PPEnv
  { ppEnvEnv :: !EnvMap,
    ppEnvConf :: !(Maybe JSON.Object)
  }

runPP ::
  PP a ->
  ArgMap ->
  PPEnv ->
  IO (Either (NonEmpty ParseError) (a, PPState))
runPP p args envVars =
  validationToEither <$> runValidationT (runStateT (runReaderT p envVars) args)

ppArg :: PP (Maybe String)
ppArg = state ArgMap.consumeArgument

ppOpt :: [Dashed] -> PP (Maybe String)
ppOpt ds = state $ ArgMap.consumeOption ds

ppSwitch :: [Dashed] -> PP (Maybe ())
ppSwitch ds = state $ ArgMap.consumeSwitch ds

ppErrors :: NonEmpty ParseError -> PP a
ppErrors = lift . lift . ValidationT . pure . Failure

ppError :: ParseError -> PP a
ppError = ppErrors . NE.singleton
