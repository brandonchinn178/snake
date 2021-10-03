module Snake.Core.Targets (
  NextTargets,
  mkNextTargets,
  findNextTargetWhere,
) where

import Data.List (unfoldr)
import System.Random (getStdGen, randomR)

import Snake.Core.Grid (
  Coordinate,
  gridHeight,
  gridWidth,
 )

-- | An infinite list of all the next targets.
newtype NextTargets = NextTargets [Coordinate]

mkNextTargets :: IO NextTargets
mkNextTargets = NextTargets . unfoldr go <$> getStdGen
  where
    go s0 = Just $
      let (x, s1) = randomR (0, gridWidth - 1) s0
          (y, s2) = randomR (0, gridHeight - 1) s1
       in ((x, y), s2)

findNextTargetWhere :: (Coordinate -> Bool) -> NextTargets -> (Coordinate, NextTargets)
findNextTargetWhere f = go
  where
    go (NextTargets coords) =
      let c = head coords
          rest = NextTargets $ tail coords
       in if f c
            then (c, rest)
            else go rest
