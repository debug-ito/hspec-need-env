module Test.Hspec.NeedEnvSpec (main, spec) where

import Control.Monad.IO.Class (liftIO)
import Data.List (isInfixOf)
import Data.IORef (IORef, newIORef, modifyIORef, readIORef)
import System.SetEnv (setEnv, unsetEnv)
import Test.Hspec
import Test.Hspec.Core.Runner (hspecWithResult, Summary(..), Config(..), defaultConfig)
import Test.Hspec.Core.Formatters (FailureReason(..), Formatter)
import qualified Test.Hspec.Core.Formatters as F
import Test.Hspec.Core.Util (Path)

import Test.Hspec.NeedEnv (needEnv)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "needEnv" $ do
    it "should get the specified env" $ do
      setEnv "HOGE" "hoge"
      successCase needEnv "HOGE" "hoge"
    it "should fail when the specified env is not present" $ do
      let needEnvStr :: String -> IO String
          needEnvStr = needEnv
      unsetEnv "HOGE"
      failCase needEnvStr "HOGE" (\msg -> "not set" `isInfixOf` msg)
  describe "wantEnv" $ do
    it "should get the specified env" $ True `shouldBe` False -- TODO
    it "should make the test pending when the specified env is not present" $ True `shouldBe` False -- TODO
    it "should fail when it fails to parse the env" $ True `shouldBe` False -- TODO

successCase :: (Read a, Show a, Eq a) => (String -> IO a) -> String -> a -> IO ()
successCase envGet envkey envval = do
  (conf, ref_results) <- configForTest
  got_summary <- hspecWithResult conf (before (envGet envkey) $ sampleSpec envval)
  got <- readIORef ref_results
  map resultBody got `shouldBe` [ExampleSuccess]
  got_summary `shouldBe` Summary { summaryExamples = 1, summaryFailures = 0 }
  

failCase :: (String -> IO a) -> String -> (String -> Bool) -> IO ()
failCase envGet envkey andExpect = do
  let expect [ExampleFailure (Just msg)] = (envkey `isInfixOf` msg) && andExpect msg
      expect _ = False
  (conf, ref_results) <- configForTest
  got_summary <- hspecWithResult conf (before (envGet envkey) sampleSuccess)
  got <- fmap (map resultBody) $ readIORef ref_results
  got `shouldSatisfy` expect
  got_summary `shouldBe` Summary { summaryExamples = 1, summaryFailures = 1 }


sampleSpec :: (Eq a, Show a) => a -> SpecWith a
sampleSpec expected = specify "dummy" $ \got -> got `shouldBe` expected

sampleSuccess :: SpecWith a
sampleSuccess = specify "dummy" $ const (True `shouldBe` True)

data ExampleResultBody = ExampleSuccess
                       | ExampleFailure (Maybe String)
                       | ExamplePending (Maybe String)
                       deriving (Show,Eq,Ord)

type ExampleResult = (Path, ExampleResultBody)

resultBody :: ExampleResult -> ExampleResultBody
resultBody (_, b) = b

failureReasonToString :: Either e FailureReason -> Maybe String
failureReasonToString (Right (Reason s)) = Just s
failureReasonToString _ = Nothing

formatterForTest :: IO (Formatter, IORef [ExampleResult])
formatterForTest = do
  ref_results <- newIORef []
  let putRet r = liftIO $ modifyIORef ref_results (\rs -> rs ++ [r])
      -- TODO: Fomatter internal changed between hspec 2.4 and 2.5
      f = F.silent { F.exampleSucceeded = \p -> putRet (p, ExampleSuccess),
                     F.exampleFailed = \p efr -> putRet (p, ExampleFailure (failureReasonToString efr)),
                     F.examplePending = \p ms -> putRet (p, ExamplePending ms)
                   }
  return (f, ref_results)

configForTest :: IO (Config, IORef [ExampleResult])
configForTest = do
  (fmt, ref_results) <- formatterForTest
  let conf = defaultConfig { configIgnoreConfigFile = True,
                             configDryRun = False,
                             configFormatter = Just fmt
                           }
  return (conf, ref_results)
