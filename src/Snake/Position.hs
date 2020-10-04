{-# LANGUAGE TemplateHaskell #-}
module Snake.Position
 ( -- * Basic
   Position(..)
 , random
 , move
 , wrap
 , build
 -- * Lenses
 , px
 , py
 ) where

import Data.List (unfoldr)
import Lens.Micro ((&), (.~), (%~), (^.))
import Lens.Micro.TH (makeLenses)
import System.Random (StdGen, uniformR)

import Snake.Direction (Direction(..))

data Position = Position
  { _px :: Int
  , _py :: Int
  } deriving Eq

makeLenses ''Position

-- | Generate random position given the ranges r1 and r2.
random :: (Int, Int) -> (Int, Int) -> StdGen -> (Position, StdGen)
random r1 r2 gen =
  let (x, g1) = uniformR r1 gen
      (y, g2) = uniformR r2 g1
  in (Position x y, g2)

-- | Wrap position using the ranges (0, maxX) and (0, maxY).
wrap :: (Int, Int) -- ^ (maxX, maxY)
     -> Position   -- ^ The position
     -> Position
wrap (maxX, maxY) pos
  | pos^.px < 0    = pos & px .~ maxX
  | pos^.px > maxX = pos & px .~ 0
  | pos^.py < 0    = pos & py .~ maxY
  | pos^.py > maxY = pos & py .~ 0
  | otherwise      = pos

-- | Move position one unit (using 'succ' and 'pred') in the specified direction.
move :: Position -> Direction -> Position
move p North = p & py %~ pred
move p South = p & py %~ succ
move p East  = p & px %~ succ
move p West  = p & px %~ pred

-- | Build a list of connected positions.
build :: Position   -- ^ Starting position, not included in the result
      -> Int        -- ^ Total length
      -> Direction  -- ^ Direction in which to build
      -> (Int, Int) -- ^ Range of (maxX, maxY) to which the built positions are 'wrap'ped
      -> [Position]
build start len dir wrapRange = unfoldr (\(p, n) ->
                                          if n == 0
                                            then Nothing
                                            else let p' = wrap wrapRange $ move p dir
                                                 in Just (p', (p', n-1)))
                                        (start, len)
