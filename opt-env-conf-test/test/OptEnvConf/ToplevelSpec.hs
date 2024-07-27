module OptEnvConf.ToplevelSpec where

import Data.Version
import OptEnvConf
import System.Environment
import System.Exit
import Test.Syd

spec :: Spec
spec = do
  let dummyVersion = makeVersion [0, 0, 0]
  let dummyProgDesc = "dummy"
  let dummyParser = pure 'a'
  let dummyRun = runParser dummyVersion dummyProgDesc dummyParser
  sequential $ do
    it "can fail fast when too many unrecognsied arguments are given" $
      withArgs ["-option1", "option", "--option2", "option", "--option3", "option", "--option4", "option"] $
        dummyRun `shouldThrow` (== ExitFailure 1)

    describe "--help" $ do
      it "can show help text with --help" $
        withArgs ["--help"] $
          dummyRun `shouldThrow` (== ExitSuccess)
      it "can show help text with --help even when there are more args" $
        withArgs ["--help", "more", "args", "here"] $
          dummyRun `shouldThrow` (== ExitSuccess)
      it "can show help text with -h" $
        withArgs ["-h"] $
          dummyRun `shouldThrow` (== ExitSuccess)
      it "can show help text with --help even when there are more args" $
        withArgs ["-h", "more", "args", "here"] $
          dummyRun `shouldThrow` (== ExitSuccess)

    describe "--version" $ do
      it "can show version info with --version" $
        withArgs ["--version"] $
          dummyRun `shouldThrow` (== ExitSuccess)

    describe "--run-settings-check" $ do
      it "can run a settings check with --run-settings-check" $
        withArgs ["--run-settings-check"] $
          dummyRun `shouldThrow` (== ExitSuccess)
      it "can run a settings check with --run-settings-check even when there are more args" $
        withArgs ["--run-settings-check", "more", "args", "here"] $
          runParser
            dummyVersion
            dummyProgDesc
            ( many
                ( setting
                    [ help "something that reads args",
                      argument,
                      reader str,
                      metavar "STR"
                    ]
                ) ::
                Parser [String]
            )
            `shouldThrow` (== ExitSuccess)
