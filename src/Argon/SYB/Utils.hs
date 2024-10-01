-- The following code is temporarily taken from @alanz's fork of
-- nominolo/ghc-syb. Argon will use the original ghc-syb when a new version
-- is released on Hackage with @alanz's fixes.
{-# LANGUAGE RankNTypes #-}

module Argon.SYB.Utils (Stage (..), everythingStaged)
where

import Data.Generics
import GHC
import GHC.Types.Name.Set (NameSet)

-- | Ghc Ast types tend to have undefined holes, to be filled
--   by later compiler phases. We tag Asts with their source,
--   so that we can avoid such holes based on who generated the Asts.
data Stage = Parser | Renamer | TypeChecker deriving (Eq, Ord, Show)

-- | Like 'everything', but avoid known potholes, based on the 'Stage' that
--   generated the Ast.
everythingStaged :: Stage -> (r -> r -> r) -> r -> GenericQ r -> GenericQ r
everythingStaged stage k z f x
  | ( const False
        `extQ` fixity
        `extQ` nameSet
    )
      x =
      z
  | otherwise = foldl k (f x) (gmapQ (everythingStaged stage k z f) x)
  where
    nameSet = const (stage `elem` [Parser, TypeChecker]) :: NameSet -> Bool
    fixity = const (stage < Renamer) :: GHC.Fixity -> Bool
