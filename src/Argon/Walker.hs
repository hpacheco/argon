{-# LANGUAGE OverloadedStrings #-}

module Argon.Walker (allFiles) where

import Control.Monad (guard)
import System.Directory (doesFileExist)
import System.FilePath.Glob qualified as Glob

-- | Starting from a path, generate a sequence of paths corresponding
--   to Haskell files. The filesystem is traversed depth-first.
allFiles :: FilePath -> IO [FilePath]
allFiles path = do
  isFile <- doesFileExist path
  if isFile
    then do
      guard $ Glob.match "**/*.hs" path
      pure [path]
    else hsFilesIn path

hsFilesIn :: FilePath -> IO [FilePath]
hsFilesIn path = Glob.globDir1 (Glob.compile "**/*.hs") path
