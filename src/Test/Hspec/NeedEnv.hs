-- |
-- Module: Test.Hspec.NeedEnv
-- Description:
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
module Test.Hspec.NeedEnv
       ( needEnv,
         wantEnv
       ) where

-- | Return value of the specified environment variable. If that
-- environment variable is not set or it fails to parse the value, it
-- signals failure in hspec.
needEnv :: Read a
        => String -- ^ name of the environment variable
        -> IO a -- ^ value of the environment variable
needEnv = undefined

-- | Like 'needEnv', but this functions signals \"pending\" in hsepc.
wantEnv :: Read a
        => String -- ^ name of the environment variable
        -> IO a -- ^ value of the environment variable
wantEnv = undefined
