{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Argon.Results (order, filterResults, filterNulls, exportStream)
where

import Data.List (sortBy)
import Data.Ord (comparing)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<*), (*>))
#endif

import Data.Aeson qualified as Aeson

import Argon.Formatters
import Argon.Types
import Data.ByteString.Lazy.Char8 qualified as BSL

-- sortOn is built-in only in base 4.8.0.0 onwards
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))

-- | Order a list of blocks. Ordering is done with respect to:
--
--     1. complexity (descending)
--     2. line number (ascending)
--     3. function name (alphabetically)
order :: [ComplexityBlock] -> [ComplexityBlock]
order = sortOn (\(CC ((l, _), f, cc)) -> (-cc, l, f))

-- | A result is discarded if it correspond to a successful analysis and there
--   are no blocks to show
filterNulls :: (FilePath, AnalysisResult) -> Bool
filterNulls (_, r) = case r of
  Left _ -> True
  Right [] -> False
  _ -> True

-- | Filter the results of the analysis, with respect to the given
--   'Config'.
filterResults
  :: Config
  -> (FilePath, AnalysisResult)
  -> (FilePath, AnalysisResult)
filterResults _ (s, Left err) = (s, Left err)
filterResults o (s, Right rs) =
  (s, Right $ order [r | r@(CC (_, _, cc)) <- rs, cc >= minCC o])

-- | Export analysis' results. How to export the data is defined by the
--   'Config' parameter.
exportStream
  :: Config
  -> [(FilePath, AnalysisResult)]
  -> IO ()
exportStream conf source =
  case outputMode conf of
    BareText -> putStrLn . unlines $ bareTextFormatter source
    Colored -> putStrLn . unlines $ coloredTextFormatter source
    JSON -> BSL.putStrLn $ Aeson.encode source
