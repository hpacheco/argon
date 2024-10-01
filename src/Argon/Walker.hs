{-# LANGUAGE OverloadedStrings #-}
module Argon.Walker (allFiles)
    where

-- import           Data.DirStream            (childOf)
import           Data.List                 (isSuffixOf)
import           Filesystem.Path.CurrentOS (decodeString, encodeString)
import           Pipes                     (ListT (..), MonadIO, Producer, each,
                                            every, liftIO, (>->))
import qualified Pipes.Prelude             as P
import           Pipes.Safe
import           System.Directory          (doesDirectoryExist, doesFileExist,
                                            pathIsSymbolicLink)
import           System.FilePath           (takeExtension)
import qualified System.FilePath.Glob as Glob

-- | Starting from a path, generate a sequence of paths corresponding
--   to Haskell files. The filesystem is traversed depth-first.
allFiles :: MonadSafe m =>FilePath -> Producer FilePath m ()
allFiles path = do
    isFile <- liftIO $ doesFileExist path
    if isFile then each [path] >-> P.filter (".hs" `isSuffixOf`)
              else every $ hsFilesIn path

hsFilesIn :: MonadSafe m => FilePath -> ListT m FilePath
hsFilesIn path = do
  fps <- liftIO $ Glob.globDir1 (Glob.compile "**/*.hs") path
  Select $ each fps
