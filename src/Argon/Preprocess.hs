-- The following code is taken and modified from ghc-exactprint, because adding
-- a dependency for just one module and then adding wrappers for that module
-- seemed excessive.
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module provides support for CPP and interpreter directives.
module Argon.Preprocess
  ( CppOptions (..)
  , defaultCppOptions
  , getPreprocessedSrc
  , setExtensions
  ) where

import GHC.Driver.DynFlags qualified as GHC
import GHC.LanguageExtensions.Type qualified as GHC
import Language.Haskell.GhclibParserEx.GHC.Driver.Session (parsePragmasIntoDynFlags)
import Language.Haskell.GhclibParserEx.GHC.Settings.Config (fakeSettings)
import Language.Preprocessor.Cpphs qualified as Cpphs

data CppOptions = CppOptions
  { cppDefine :: [String]
  -- ^ CPP #define macros
  , cppInclude :: [FilePath]
  -- ^ CPP Includes directory
  , cppFile :: [FilePath]
  -- ^ CPP pre-include file
  }

defaultCppOptions :: CppOptions
defaultCppOptions = CppOptions [] [] []

setExtensions :: ([GHC.Extension], [GHC.Extension]) -> FilePath -> String -> IO (Either String GHC.DynFlags)
setExtensions = parsePragmasIntoDynFlags baseDynFlags
  where
    baseDynFlags = GHC.defaultDynFlags fakeSettings

getPreprocessedSrc :: CppOptions -> FilePath -> String -> IO String
getPreprocessedSrc opts = Cpphs.runCpphs opts'
  where
    opts' :: Cpphs.CpphsOptions
    opts' =
      Cpphs.defaultCpphsOptions
        { Cpphs.includes = opts.cppInclude
        -- , Cpphs.defines = opts.cppDefine
        }
