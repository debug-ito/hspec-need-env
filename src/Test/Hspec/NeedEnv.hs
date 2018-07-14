-- |
-- Module: Test.Hspec.NeedEnv
-- Description:
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
module Test.Hspec.NeedEnv
       ( needEnv,
         wantEnv
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
   Nothing -> failAndError ("Environment variable " <> envkey <> " is not set.")
   Just str -> case readEither str of
     Left e -> failAndError ("Cannot parse environment variable " <> envkey <> " = " <> str <> ": " <> e)
     Right val -> return val

-- | 'error' is necessary for arbitrary return type.
failAndError :: String -> IO a
failAndError msg = expectationFailure msg >> error msg

-- | Like 'needEnv', but this functions signals \"pending\" in hsepc.
wantEnv :: String -- ^ name of the environment variable
        -> IO String -- ^ value of the environment variable
wantEnv = undefined
