-- |
-- Module: Test.Hspec.NeedEnv
-- Description:
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
module Test.Hspec.NeedEnv
       ( EnvMode(..),
         needEnv,
         needEnvParse,
         needEnvRead,
       ) where

import Data.Monoid ((<>))
import System.Environment (lookupEnv)
import Test.Hspec.Expectations (expectationFailure)
import Text.Read (readEither)

-- | How to treat missing environment variable.
data EnvMode = Need
               -- ^ If the environment variable is not set, the test
               -- fails.
             | Want
               -- ^ If the environment variable is not set, the test
               -- gets pending.
             deriving (Show,Eq,Ord,Enum,Bounded)

-- | Return value of the specified environment variable.
needEnv :: EnvMode
        -> String -- ^ name of the environment variable
        -> IO String -- ^ value of the environment variable
needEnv _ envkey = do
  mval <- lookupEnv envkey
  case mval of
   Nothing -> do
     expectationFailure ("Environment variable " <> envkey <> " is not set.")
     return ""
   Just str -> return str

-- | Get environment variable by 'needEnv', and parse the value.
needEnvParse :: EnvMode
             -> (String -> Either String a) -- ^ the parse of the environment variable
             -> String
             -> IO a
needEnvParse mode parseEnvVal envkey = do
  val_str <- needEnv mode envkey
  case parseEnvVal val_str of
   Right val -> return val
   Left e -> do
     let error_msg = "Fail to parse environment variable " <> envkey <> ": " <> e
     expectationFailure error_msg
     error error_msg

-- | Parse the environment variable with 'Read' class.
needEnvRead :: (Read a)
            => EnvMode -> String -> IO a
needEnvRead mode = needEnvParse mode readEither
