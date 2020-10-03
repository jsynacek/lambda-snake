{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (forever, void)
import Control.Concurrent (forkIO, threadDelay)
import Data.List (delete)

import Brick.AttrMap
import Brick.BChan
import Brick.Main
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
  , _score :: Integer
  , _stdgen :: StdGen
  }

makeLenses ''Position
makeLenses ''Direction
makeLenses ''Snake
makeLenses ''Game

data Name = SomeName deriving (Eq, Ord)

data GameEvent = Tick

data Cell = Head | Body | Food | Empty

initSnake :: Snake
initSnake = Snake { _body = [Position x 10 | x <- [10, 9..3]]
                  , _direction = East
                  }

moveSnake :: Game -> Maybe Game
moveSnake game = if collision then Nothing else Just game'
  where
    collision = newHd `elem` newTail
    ateFood = newHd `elem` game^.food
    -- TODO: Figure out why the pattern doesn't need brackets around it...
    Snake (hd:tl) dir = game^.snake
    newHd = case dir of
      North -> hd & py -~ 1 & wrap
      South -> hd & py +~ 1 & wrap
      East -> hd & px +~ 1 & wrap
      West -> hd & px -~ 1 & wrap
    maxX = gameWidth - 1
    maxY = gameHeight - 1
    wrap pos
      | pos^.px < 0 = pos & px .~ maxX
      | pos^.px > maxX = pos & px .~ 0
      | pos^.py < 0 = pos & py .~ maxY
      | pos^.py > maxY = pos & py .~ 0
      | otherwise = pos
    newTail = if ateFood
                then hd : tl
                else hd : init tl -- TODO: performance???
    game' = if ateFood
              then game & snake . body .~ newHd:newTail
                        & food %~ delete newHd
              else game & snake . body .~ newHd:newTail

gameWidth, gameHeight, gameMaxFood :: Int
gameWidth = 50
gameHeight = 20
gameMaxFood = 3

initGame :: StdGen -> Game
initGame gen = Game { _snake = initSnake
                    , _food = []
                    , _score = 0
                    , _stdgen = gen
                    }

spawnFood :: Game -> Game
spawnFood game =
  let obstructed = game^.snake^.body
      -- TODO/FIXME: This is stupid and going to be slow when snake gets very long.
      --             Figure out something better.
      genFood 0 _ fd = fd
      genFood n g fd = let (x, g2) = uniformR (0, gameWidth-1) g
                           (y, g3) = uniformR (0, gameHeight-1) g2
                       in if (Position x y) `notElem` obstructed
                            then genFood (n-1) g3 (Position x y : fd)
                            else genFood n g3 fd
  in game & food .~ genFood gameMaxFood (game^.stdgen) []


tickGame :: Game -> Maybe Game
tickGame game = moveSnake game'
  where
    game' = if (null $ game^.food)
              then spawnFood game
              else game

drawCell :: Cell -> Widget Name
drawCell Head = str "λ"
drawCell Body = str "○"
drawCell Food = str "✦"
drawCell Empty = str " "

-- TODO/FIXME: Now this is SLOW AS FUCK! See profiler output...
drawGame :: Game -> [Widget Name]
drawGame (Game (Snake (hd:tl) _) fd _ _) = [border $ vBox [drawRow ry | ry <- [0..gameHeight-1]]]
  where
    drawRow ry = hBox [cell cx ry | cx <- [0..gameWidth-1]]
    cell cx cy
      | cx == hd^.px && cy == hd^.py = drawCell Head
      | (Position cx cy) `elem` tl = drawCell Body
      | (Position cx cy) `elem` fd = drawCell Food
      | otherwise = drawCell Empty

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
