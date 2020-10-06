{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (forever, void)
import Control.Concurrent (forkIO, threadDelay)
import Data.List (delete)
import Text.Printf (printf)

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

import           Snake.Direction (Direction(..))
import qualified Snake.Direction as D
import           Snake.Position (Position(..), px, py)
import qualified Snake.Position as P

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

data GameEvent = Tick

data Cell = Head | Body | Food | Wall | Empty

makeLenses ''Snake
makeLenses ''Game

gameWidth, gameHeight, gameMaxFood, gameMaxX, gameMaxY :: Int
gameWidth = 50
gameHeight = 20
gameMaxFood = 3
gameMaxX = gameWidth - 1
gameMaxY = gameHeight - 1

initSnake :: Snake
initSnake = Snake { _body = [Position x 10 | x <- [10, 9, 8]]
                  , _direction = East
                  }

genPosition :: StdGen -> (Position, StdGen)
genPosition = P.random (0, gameMaxX) (0, gameMaxY)

buildWall :: Int -> Direction -> StdGen -> ([Position], StdGen)
buildWall total to gen = go total to ([], gen)
  where
    maxPartLen = total `div` 2
    go 0 _ res = res
    go l d (wls, g) = let (pos, g1) = if null wls then genPosition g else (last wls, gen)
                          (len, g2) = uniformR (1, min l maxPartLen) g1
                          (dir, g3) = D.random d g2
                      in go (l-len) dir (wls ++ P.build pos len dir (gameMaxX, gameMaxY), g3)

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
      genFood 0 g fd = (fd, g)
      genFood n g fd = let (pos, g') = genPosition g
                       in if pos `notElem` obstructed
                            then genFood (n-1) g' (pos : fd)
                            else genFood n g' fd
      (newFood, newGen) = genFood gameMaxFood (game^.stdgen) []
  in game & food .~ newFood
          & stdgen .~ newGen

tickSnake :: Game -> Maybe Game
tickSnake game = if collision then Nothing else Just game'
  where
    collision = newHd `elem` game^.walls || newHd `elem` newTail
    ateFood = newHd `elem` game^.food
    Snake (hd:tl) dir = game^.snake
    newHd = P.wrap (gameMaxX, gameMaxY) $ P.move hd dir
    newTail = if ateFood
                then hd : tl
                else if null tl then [] else hd : init tl -- TODO: performance???
    game' = if ateFood
              then game & snake . body .~ newHd:newTail
                        & food %~ delete newHd
                        & score %~ succ
              else game & snake . body .~ newHd:newTail


tickGame :: Game -> Maybe Game
tickGame game = tickSnake game'
  where
    game' = if (null $ game^.food)
              then spawnFood game
              else game

-- TODO/FIXME: Now this is still slow, because it constructs/renders
--             the entire board on every tick.
drawGame :: Game -> [Widget ()]
drawGame game = [hCenter $ vBox [ border $ vBox [drawRow ry | ry <- [0..gameMaxY]]
                                                , padLeft (Pad $ gameMaxX-pad) .
                                                  borderWithLabel (str "score") .
                                                    str . printf "%7v" $ game^.score
                                                ]]
  where
    Snake (hd:tl) _ = game^.snake
    pad = 6
    drawRow ry = str [cell cx ry | cx <- [0..gameMaxX]]
    cell cx cy
      | cx == hd^.px && cy == hd^.py = drawCell Head
      | pos `elem` tl = drawCell Body
      | pos `elem` game^.food = drawCell Food
      | pos `elem` game^.walls = drawCell Wall
      | otherwise = drawCell Empty
      where
        pos = Position cx cy
    drawCell Head = 'λ'
    drawCell Body = '○'
    drawCell Food = '✦'
    drawCell Wall = '⯀'
    drawCell Empty = ' '


handleGameEvent :: Game -> BrickEvent () GameEvent -> EventM () (Next Game)
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
    changeDirection dir = if (D.opposite dir /= game^.snake^.direction)
                            then game & snake . direction .~ dir
                            else game

app :: App Game GameEvent ()
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
