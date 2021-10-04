module Snake.Core.Targets (
  NextTargets,
  mkNextTargets,
  findNextTargetWhere,
) where

import Data.List (unfoldr)
import System.Random (getStdGen)
import System.Random.Stateful (runSTGen, uniformRM)

import Snake.Core.Grid (
  Coordinate,
  gridHeight,
  gridWidth,
 )

-- | An infinite list of all the next targets.
newtype NextTargets = NextTargets [Coordinate]

mkNextTargets :: IO NextTargets
mkNextTargets = NextTargets . unfoldr (Just . go) <$> getStdGen
  where
    go s = runSTGen s $ \g -> do
      x <- uniformRM (0, gridWidth - 1) g
      y <- uniformRM (0, gridHeight - 1) g
      pure (x, y)

findNextTargetWhere :: (Coordinate -> Bool) -> NextTargets -> (Coordinate, NextTargets)
findNextTargetWhere f = go
  where
    go (NextTargets coords) =
      let c = head coords
          rest = NextTargets $ tail coords
       in if f c
            then (c, rest)
            else go rest
