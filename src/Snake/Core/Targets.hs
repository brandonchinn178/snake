{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Snake.Core.Targets (
  NextTargets,
  mkNextTargets,
  findNextTargetWhere,
) where

import Data.List (unfoldr)
import System.Random (getStdGen)
import System.Random.Stateful (runSTGen, uniformRM)

import Snake.Core.Grid (Coordinate, Grid (..))

-- | An infinite list of all the next targets.
newtype NextTargets = NextTargets [Coordinate]

mkNextTargets :: Grid -> IO NextTargets
mkNextTargets Grid{..} = NextTargets . unfoldr (Just . go) <$> getStdGen
  where
    go s = runSTGen s $ \g -> do
      x <- uniformRM (0, gridWidth - 1) g
      y <- uniformRM (0, gridHeight - 1) g
      pure (x, y)

getNextTarget :: NextTargets -> (Coordinate, NextTargets)
getNextTarget = \case
  NextTargets (coord:rest) -> (coord, NextTargets rest)
  NextTargets [] -> error "Invariant violation: NextTargets was empty"

findNextTargetWhere :: (Coordinate -> Bool) -> NextTargets -> (Coordinate, NextTargets)
findNextTargetWhere f = go
  where
    go nextTargets =
      let (c, rest) = getNextTarget nextTargets
       in if f c
            then (c, rest)
            else go rest
