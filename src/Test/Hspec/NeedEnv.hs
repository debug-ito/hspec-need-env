-- |
-- Module: Test.Hspec.NeedEnv
-- Description:
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
module Test.Hspec.NeedEnv
       ( -- * needEnv
         needEnv,
         needEnvParse,
         needEnvRead,
         -- * wantEnv
         wantEnv,
         wantEnvParse,
         wantEnvRead
       ) where

import Data.Monoid ((<>))
import System.Environment (lookupEnv)
import Test.Hspec.Expectations (expectationFailure)
import Text.Read (readEither)

-- | Return value of the specified environment variable. If that
-- environment variable is not set, it signals failure in hspec.
needEnv :: String -- ^ name of the environment variable
        -> IO String -- ^ value of the environment variable
needEnv envkey = do
  mval <- lookupEnv envkey
  case mval of
   Nothing -> do
     expectationFailure ("Environment variable " <> envkey <> " is not set.")
     return ""
   Just str -> return str

-- | Get environment variable by 'needEnv', and parse the value.
needEnvParse :: (String -> Either String a) -- ^ the parse of the environment variable
             -> String -> IO a
needEnvParse parseEnvVal envkey = do
  val_str <- needEnv envkey
  case parseEnvVal val_str of
   Right val -> return val
   Left e -> do
     let error_msg = "Fail to parse environment variable " <> envkey <> ": " <> e
     expectationFailure error_msg
     error error_msg

-- | Parse the environment variable with 'Read' class.
needEnvRead :: (Read a) => String -> IO a
needEnvRead = needEnvParse readEither

-- | Like 'needEnv', but this functions signals \"pending\" in hsepc.
wantEnv :: String -- ^ name of the environment variable
        -> IO String -- ^ value of the environment variable
wantEnv = undefined

-- | Get environment variable by 'wantEnv', and parse the value.
wantEnvParse :: (String -> Either String a) -> String -> IO a
wantEnvParse = undefined

-- | Parse the environment variable with 'Read' class.
wantEnvRead :: (Read a) => String -> IO a
wantEnvRead = wantEnvParse readEither
