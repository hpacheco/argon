{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Argon.Types
  ( ComplexityBlock (CC)
  , AnalysisResult
  , Config (..)
  , OutputMode (..)
  , GhcParseError (..)
  , defaultConfig
  , Argon (..)
  )
where

import Control.Exception (Exception)
import Data.Aeson
import Data.List (intercalate)
import Data.Typeable

import Argon.Loc
import Data.List.NonEmpty (NonEmpty)

data GhcParseError = GhcParseError
  { loc :: Loc
  , msg :: String
  }
  deriving (Typeable)

-- | Hold the data associated to a function binding:
--   @(location, function name, complexity)@.
newtype ComplexityBlock = CC (Loc, String, Int)
  deriving (Show, Eq, Ord)

-- | Represent the result of the analysis of one file.
--   It can either be an error message or a list of
--   'ComplexityBlock's.
type AnalysisResult = Either String [ComplexityBlock]

data Argon = Argon
  { config :: Config
  , paths :: NonEmpty FilePath
  } deriving (Show)

-- | Type holding all the options passed from the command line.
data Config = Config
  { minCC :: Int
  -- ^ Minimum complexity a block has to have to be shown in results.
  , exts :: [String]
  -- ^ Extension to activate
  , headers :: [FilePath]
  -- ^ Header files to be automatically included before preprocessing
  , includeDirs :: [FilePath]
  -- ^ Additional include directories for the C preprocessor
  , outputMode :: OutputMode
  -- ^ Describe how the results should be exported.
  } deriving (Show)

-- | Type describing how the results should be exported.
data OutputMode
  = -- | Text-only output, no colors.
    BareText
  | -- | Text-only output, with colors.
    Colored
  | -- | Data is serialized to JSON.
    JSON
  deriving (Show, Eq)

-- | Default configuration options.
--
--   __Warning__: These are not Argon's default options.
defaultConfig :: Config
defaultConfig =
  Config
    { minCC = 1
    , exts = []
    , headers = []
    , includeDirs = []
    , outputMode = JSON
    }

instance Exception GhcParseError

instance Show GhcParseError where
  show e = tagMsg (loc e) $ fixNewlines (msg e)
    where
      fixNewlines = intercalate "\n\t\t" . lines

instance ToJSON ComplexityBlock where
  toJSON (CC ((s, c), func, cc)) =
    object
      [ "lineno" .= s
      , "col" .= c
      , "name" .= func
      , "complexity" .= cc
      ]

instance {-# OVERLAPPING #-} ToJSON (FilePath, AnalysisResult) where
  toJSON (p, Left err) =
    object
      [ "path" .= p
      , "type" .= ("error" :: String)
      , "message" .= err
      ]
  toJSON (p, Right rs) =
    object
      [ "path" .= p
      , "type" .= ("result" :: String)
      , "blocks" .= rs
      ]
