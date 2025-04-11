module Main where

import Control.Monad.IO.Class (MonadIO)
import Data.List (find)
import Data.Maybe (isJust, isNothing)
import System.IO
import System.Random (Random (..), getStdRandom)

type GameState = [[Integer]]

getGameStateStr :: GameState -> String
getGameStateStr state = getGameStateStr_ state ""
 where
  getGameStateStr_ :: GameState -> String -> String
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

putNewTile :: (Int, Int, Integer) -> GameState -> GameState
putNewTile (y, x, value) = mapi (\row rowi -> mapi (\val vali -> if x == vali && y == rowi then value else val) row)

makeMove :: (MonadIO m) => Input -> GameState -> m GameState
makeMove input state = do
  let state2 = processInput input state
  newTileLoc <- randomPlace state
  let nextState = putNewTile newTileLoc state2
  return nextState

processInput :: Input -> GameState -> GameState
processInput input state = state

gameStateGet :: GameState -> (Int, Int) -> Integer
gameStateGet state (x, y) = (state !! x) !! y

getRandomLoc :: (MonadIO m) => [[Integer]] -> m (Int, Int)
getRandomLoc state = do
  x <- randRange 0 3
  y <- randRange 0 3
  let val = gameStateGet state (x, y)
  if val == 0
    then return (x, y)
    else getRandomLoc state

randomPlace :: (MonadIO m) => GameState -> m (Int, Int, Integer)
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

  let gameOver = isNothing $ find (isJust . find (== 0)) nextState

  if gameOver
    then putStrLn "game over"
    else mainGameLoop nextState

initialState :: GameState
initialState = replicate 4 $ replicate 4 0

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
  mainGameLoop initialState
  putStrLn "Thanks for playing"
