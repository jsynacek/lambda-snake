module Snake.Direction
  ( Direction(..)
  , random
  , opposite
  ) where

import System.Random (StdGen, uniformR)

data Direction = North | South | East | West deriving Eq

-- | Generate new direction from the current one.
--   The new direction is always chosen so that it either stays the same
--   or turns by 90 degrees.
random :: Direction -- ^ The current direction
       -> StdGen
       -> (Direction, StdGen)
random dir gen = (newDir, newGen)
  where
    (idx, newGen) = uniformR (0, 2) gen
    newDir = case dir of
               North -> [North, East, West]  !! idx
               South -> [South, East, West]  !! idx
               East  -> [East, North, South] !! idx
               West  -> [West, North, South] !! idx

opposite :: Direction -> Direction
opposite North = South
opposite South = North
opposite East = West
opposite West = East
