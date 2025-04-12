module Main where

import Control.Monad.IO.Class (MonadIO)
import Data.List (find, transpose)
import Data.Maybe (isJust, isNothing)
import System.IO
import System.Random (Random (..), getStdRandom)

-- type GameState = [[Integer]]
--
type Grid = [[Integer]]

data GameState = GameState
  { grid :: Grid
  , score :: Integer
  }
  deriving (Show, Eq)

getGameStateStr :: GameState -> String
getGameStateStr state = getGameStateStr_ (grid state) ""
 where
  getGameStateStr_ :: Grid -> String -> String
  getGameStateStr_ rest acc = case rest of
    [] -> acc
    row : rows -> getRow_ row ++ "\n" ++ getGameStateStr_ rows acc
   where
    getRow_ :: [Integer] -> String
    getRow_ = concatMap show

data Input = InUp | InLeft | InRight | InDown deriving (Show, Eq)

getInput :: IO Input
getInput = do
  rawIn <- putStr "in: " >>= const getChar
  let val =
        ( case rawIn of
            'w' -> Just InUp
            'd' -> Just InRight
            's' -> Just InDown
            'a' -> Just InLeft
            _ -> Nothing
        )
  putStr "\n"

  maybe getInput return val

placeNewRandomTile :: (MonadIO m) => Grid -> m Grid
placeNewRandomTile currGrid = do
  (y, x) <- getRandomLoc currGrid
  newTileVal <- randRange 1 2
  return (mapi (\row rowi -> mapi (\val vali -> if x == vali && y == rowi then newTileVal * 2 else val) row) currGrid)

makeMove :: (MonadIO m) => Input -> GameState -> m GameState
makeMove input startState = do
  let movedState = processInput input startState
  gridWithRandomPlacement <- placeNewRandomTile (grid movedState)
  return $ GameState gridWithRandomPlacement (score movedState)

rotateGrid :: Input -> Grid -> Grid
rotateGrid input state = case input of
  InRight -> map reverse state
  InLeft -> state
  InDown -> transpose state
  InUp -> transpose $ map reverse state

rotateGridBack :: Input -> Grid -> Grid
rotateGridBack input state = case input of
  InRight -> map reverse state
  InLeft -> state
  InDown -> transpose state
  InUp -> map reverse $ transpose state

rowMergers :: [Integer] -> [Integer]
rowMergers row = case row of
  x : y : xs | x /= 0 && x == y -> (x * 2) : rowMergers xs ++ [0]
  x : xs -> x : rowMergers xs
  [] -> []

moveRow :: [Integer] -> [Integer]
moveRow row = case row of
  x : xs
    | x == 0 -> moveRow xs ++ [x]
    | otherwise -> x : moveRow xs
  [] -> []

processInput :: Input -> GameState -> GameState
processInput input state = do
  let rotatedState = rotateGrid input (grid state)
  let movedState = map moveRow rotatedState
  let mergedState = map rowMergers movedState -- todo calc new score
  GameState (rotateGridBack input mergedState) (score state)

gameStateGet :: Grid -> (Int, Int) -> Integer
gameStateGet state (x, y) = (state !! x) !! y

getRandomLoc :: (MonadIO m) => Grid -> m (Int, Int)
getRandomLoc state = do
  x <- randRange 0 3
  y <- randRange 0 3
  let val = gameStateGet state (x, y)
  if val == 0
    then return (x, y)
    else getRandomLoc state

randomPlace :: (MonadIO m) => Grid -> m (Int, Int, Integer)
randomPlace state = do
  (x, y) <- getRandomLoc state
  val <- randRange 1 2
  return (x, y, val * 2)

randRange :: (MonadIO m, Random a) => a -> a -> m a
randRange lo hi = getStdRandom (randomR (lo, hi))

mainGameLoop :: GameState -> IO ()
mainGameLoop state = do
  putStrLn $ getGameStateStr state
  inputValue <- getInput
  print $ show inputValue
  nextState <- makeMove inputValue state

  let gameOver = isNothing $ find (isJust . find (== 0)) (grid nextState)

  if gameOver
    then putStrLn "game over"
    else mainGameLoop nextState

initialState :: (MonadIO m) => m GameState
initialState = do
  let initalGrid = replicate 4 $ replicate 4 0
  firstPlacement <- placeNewRandomTile initalGrid
  seccondPlacement <- placeNewRandomTile firstPlacement
  return $ GameState seccondPlacement 0

mapi :: (a -> Int -> a) -> [a] -> [a]
mapi mapFn lst = mapi_ lst 0
 where
  mapi_ [] _ = []
  mapi_ (x : xs) i = mapFn x i : mapi_ xs (i + 1)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to 2048-hs"
  state <- initialState
  mainGameLoop state
  putStrLn "Thanks for playing"
