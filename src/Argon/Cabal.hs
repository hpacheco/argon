{-# LANGUAGE CPP #-}

module Argon.Cabal (parseExts)
where

import Data.List (nub)

import Distribution.Utils.Path qualified as Dist
import Distribution.PackageDescription qualified as Dist
import Distribution.Simple.PackageDescription qualified as Dist
import Distribution.Verbosity qualified as Dist
import Language.Haskell.Extension qualified as Dist

-- | Parse the given Cabal file generate a list of GHC extension flags. The
--   extension names are read from the default-extensions field in the library
--   section.
parseExts :: FilePath -> IO [String]
parseExts path = extract <$> Dist.readGenericPackageDescription Dist.silent Nothing (Dist.makeSymbolicPath path)
  where
    extract pkg = maybe [] (extFromBI . Dist.libBuildInfo . Dist.condTreeData) (Dist.condLibrary pkg)

extFromBI :: Dist.BuildInfo -> [String]
extFromBI binfo = map toString . nub $ allExts
  where
    toString (Dist.UnknownExtension ext) = ext
    toString (Dist.EnableExtension ext) = show ext
    toString (Dist.DisableExtension ext) = show ext
    allExts =
      concatMap
        ($ binfo)
        [Dist.defaultExtensions, Dist.otherExtensions, Dist.oldExtensions]
