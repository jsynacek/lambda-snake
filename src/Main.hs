{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (forever, void)
import Control.Concurrent (forkIO, threadDelay)
import Data.List (delete, unfoldr)

import Brick.AttrMap
import Brick.BChan
import Brick.Main
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Core
import Brick.Types hiding (Direction)
import Graphics.Vty
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import System.Random (StdGen, getStdGen, uniformR)

data Position = Position
  { _px :: Int
  , _py :: Int
  } deriving (Eq)

data Direction = North | South | East | West deriving Eq

data Snake = Snake
  { _body :: [Position] -- TODO: Must never be empty. Use NonEmpty??
  , _direction :: Direction
  }

data Game = Game
  { _snake :: Snake
  , _food :: [Position]
  , _walls :: [Position]
  , _score :: Integer
  , _stdgen :: StdGen
  }

makeLenses ''Position
makeLenses ''Direction
makeLenses ''Snake
makeLenses ''Game

data Name = SomeName deriving (Eq, Ord)

data GameEvent = Tick

data Cell = Head | Body | Food | Wall | Empty

initSnake :: Snake
initSnake = Snake { _body = [Position x 10 | x <- [10, 9, 8]]
                  , _direction = East
                  }

moveSnake :: Game -> Maybe Game
moveSnake game = if collision then Nothing else Just game'
  where
    collision = newHd `elem` game^.walls || newHd `elem` newTail
    ateFood = newHd `elem` game^.food
    Snake (hd:tl) dir = game^.snake
    newHd = wrapPosition (gameWidth-1, gameHeight-1) $ movePosition hd dir
    newTail = if ateFood
                then hd : tl
                else if null tl then [] else hd : init tl -- TODO: performance???
    game' = if ateFood
              then game & snake . body .~ newHd:newTail
                        & food %~ delete newHd
                        & score %~ succ
              else game & snake . body .~ newHd:newTail

gameWidth, gameHeight, gameMaxFood :: Int
gameWidth = 50
gameHeight = 20
gameMaxFood = 3

-- TODO: Move position stuff to a module. generalize genPosition to take range arguments
genPosition :: StdGen -> (Position, StdGen)
genPosition g =
  let (x, g1) = uniformR (0, gameWidth-1) g
      (y, g2) = uniformR (0, gameHeight-1) g1
  in (Position x y, g2)

-- randomPosition :: ...

wrapPosition :: (Int, Int) -> Position -> Position
wrapPosition (maxX, maxY) pos
  | pos^.px < 0    = pos & px .~ maxX
  | pos^.px > maxX = pos & px .~ 0
  | pos^.py < 0    = pos & py .~ maxY
  | pos^.py > maxY = pos & py .~ 0
  | otherwise      = pos

movePosition :: Position -> Direction -> Position
movePosition p North = p & py %~ pred
movePosition p South = p & py %~ succ
movePosition p East  = p & px %~ succ
movePosition p West  = p & px %~ pred

buildPositions :: Position -> Int -> Direction -> [Position]
buildPositions start len dir = unfoldr (\(p, n) ->
                                         if n == 0
                                           then Nothing
                                           else let p' = wrapPosition (gameWidth-1, gameHeight-1) $ movePosition p dir
                                                in Just (p', (p', n-1)))
                                       (start, len)

randomDirection :: Direction -> StdGen -> (Direction, StdGen)
randomDirection dir gen = (newDir, newGen)
  where
    (idx, newGen) = uniformR (0, 2) gen
    newDir = case dir of
               North -> [North, East, West]  !! idx
               South -> [South, East, West]  !! idx
               East  -> [East, North, South] !! idx
               West  -> [West, North, South] !! idx

buildWall :: Int -> Direction -> StdGen -> ([Position], StdGen)
buildWall total to gen = go total to ([], gen)
  where
    maxPartLen = total `div` 2
    go 0 d res = res
    go l d (wls, g) = let (pos, g1) = if null wls then genPosition g else (last wls, gen)
                          (len, g2) = uniformR (1, min l maxPartLen) g1
                          (dir, g3) = randomDirection d g2
                      in go (l-len) dir (wls ++ buildPositions pos len dir, g3)


initGame :: StdGen -> Game
initGame gen = Game { _snake = initSnake
                    , _food = []
                    , _walls = wall1 ++ wall2
                    , _score = 0
                    , _stdgen = g2
                    }
  where (wall1, g1) = buildWall 10 North gen
        (wall2, g2) = buildWall 7 East g1

spawnFood :: Game -> Game
spawnFood game =
  let obstructed = game^.walls ++ game^.snake^.body
      -- TODO/FIXME: This is stupid and going to be slow when snake gets very long.
      --             Figure out something better.
      genFood 0 _ fd = fd
      genFood n g fd = let (pos, g') = genPosition g
                       in if pos `notElem` obstructed
                            then genFood (n-1) g' (pos : fd)
                            else genFood n g' fd
  in game & food .~ genFood gameMaxFood (game^.stdgen) []


tickGame :: Game -> Maybe Game
tickGame game = moveSnake game'
  where
    game' = if (null $ game^.food)
              then spawnFood game
              else game

-- TODO/FIXME: Now this is still slow, because it constructs/renders
--             the entire board on every tick.
drawGame :: Game -> [Widget Name] -- TODO: remove the pattern match and use lenses + pattern match to focus on Snake (hd:tl) only....
drawGame (Game (Snake (hd:tl) _) fd wl scr _) = [hCenter $ vBox [ border $ vBox [drawRow ry | ry <- [0..gameHeight-1]]
                                                                , padLeft (Pad $ gameWidth-1-pad) .
                                                                    borderWithLabel (str "score") .
                                                                      padLeft (Pad pad) . str $ show scr
                                                                ]]
  where
    pad = 6
    drawRow ry = str [cell cx ry | cx <- [0..gameWidth-1]]
    cell cx cy
      | cx == hd^.px && cy == hd^.py = drawCell Head
      | (Position cx cy) `elem` tl = drawCell Body
      | (Position cx cy) `elem` fd = drawCell Food
      | (Position cx cy) `elem` wl = drawCell Wall
      | otherwise = drawCell Empty
    drawCell Head = 'λ'
    drawCell Body = '○'
    drawCell Food = '✦'
    drawCell Wall = '⯀'
    drawCell Empty = ' '


handleGameEvent :: Game -> BrickEvent Name GameEvent -> EventM Name (Next Game)
handleGameEvent game e =
  case e of
    AppEvent Tick -> case tickGame game of
      Just g -> continue g
      Nothing -> halt game
    VtyEvent (EvKey KUp _) -> continue $ changeDirection North
    VtyEvent (EvKey KDown _) -> continue $ changeDirection South
    VtyEvent (EvKey KRight _) -> continue $ changeDirection East
    VtyEvent (EvKey KLeft _) -> continue $ changeDirection West
    VtyEvent (EvKey KEsc _) -> halt game
    _ -> continue game
  where
    oldDir = game^.snake^.direction
    changeDirection dir = if (opposite dir /= oldDir)
                              then game & snake . direction .~ dir
                              else game
    opposite North = South
    opposite South = North
    opposite East = West
    opposite West = East

app :: App Game GameEvent Name
app = App { appDraw = drawGame
          , appChooseCursor = const $ pure Nothing
          , appHandleEvent = handleGameEvent
          , appStartEvent = pure
          , appAttrMap = const $ attrMap defAttr []
          }

main :: IO ()
main = do
  vty <- standardIOConfig >>= mkVty
  ch <- newBChan 64
  -- TODO: So, the problem with this is that the game ticks come once every while, while the
  -- keyboard events come whenever the keyboard is pressed. Which means that, for example,
  -- the snake's direction could be change twice in a row, effectively making a 180 turn,
  -- which ends the game. Figure out a fix for that.
  void $ forkIO $ forever $ do
    writeBChan ch Tick
    threadDelay 100000
  gen <- getStdGen
  void $ customMain vty (standardIOConfig >>= mkVty) (Just ch) app (initGame gen)
