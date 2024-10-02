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
  -- , getPreprocessedSrcDirect
  ) where

import Control.Monad.IO.Class (liftIO)
import GHC.Driver.Env.Types qualified as GHC
import GHC.Driver.Phases qualified as GHC
import GHC.Settings qualified as GHC
import GHC.Types.SourceFile qualified as GHC
import Lens.Micro (Lens')
import Lens.Micro qualified as Lens
import Language.Haskell.GhclibParserEx.GHC.Driver.Session (parsePragmasIntoDynFlags)
import qualified GHC.LanguageExtensions.Type as GHC
import qualified GHC.Driver.DynFlags as GHC
import Language.Haskell.GhclibParserEx.GHC.Settings.Config (fakeSettings)
import qualified Language.Preprocessor.Cpphs as Cpphs

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
    opts' = Cpphs.defaultCpphsOptions
      { Cpphs.includes = opts.cppInclude
      -- , Cpphs.defines = opts.cppDefine
      }

-- getPreprocessedSrcDirect
--   :: (GHC.GhcMonad m, MonadFail m)
--   => CppOptions
--   -> FilePath
--   -> m (String, GHC.DynFlags)
-- getPreprocessedSrcDirect cppOptions file = do
--   hscEnv <- GHC.getSession
--   let dfs = GHC.hsc_dflags hscEnv
--       newEnv = hscEnv {GHC.hsc_dflags = injectCppOptions cppOptions dfs}
--   Right (dflags', hspp_fn) <-
--     liftIO $ GHC.preprocess newEnv file Nothing (Just (GHC.Cpp GHC.HsSrcFile))
--   txt <- liftIO $ readFile hspp_fn
--   return (txt, dflags')
-- 
-- injectCppOptions :: CppOptions -> GHC.DynFlags -> GHC.DynFlags
-- injectCppOptions CppOptions {..} dflags =
--   foldr
--     addOptP
--     dflags
--     ( map mkDefine cppDefine
--         ++ map mkIncludeDir cppInclude
--         ++ map mkInclude cppFile
--     )
--   where
--     mkDefine = ("-D" ++)
--     mkIncludeDir = ("-I" ++)
--     mkInclude = ("-include" ++)
-- 
-- toolSettings :: Lens' GHC.DynFlags GHC.ToolSettings
-- toolSettings = Lens.lens GHC.toolSettings (\dflags ts -> dflags {GHC.toolSettings = ts})
-- 
-- optP :: Lens' GHC.ToolSettings [String]
-- optP = Lens.lens GHC.toolSettings_opt_P (\ts opts -> ts {GHC.toolSettings_opt_P = opts})
-- 
-- addOptP :: String -> GHC.DynFlags -> GHC.DynFlags
-- addOptP f = Lens.over (toolSettings . optP) (f :)
