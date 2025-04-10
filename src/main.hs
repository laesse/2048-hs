module Main where

import Data.Maybe (fromMaybe, isNothing)
import System.Random (StdGen)

type GameState = [[Int]]

getGameStateStr :: GameState -> String
getGameStateStr state = getGameStateStr_ state ""
 where
  getGameStateStr_ :: GameState -> String -> String
  getGameStateStr_ rest acc = case rest of
    [] -> acc
    row : rows -> getRow_ row ++ "\n" ++ getGameStateStr_ rows acc
   where
    getRow_ :: [Int] -> String
    getRow_ = concatMap show

data Input = InUp | InLeft | InRight | InDown deriving (Show, Eq)

getInput :: IO Input
getInput = do
  putStr "in: "
  rawIn <- getChar
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

makeMove :: Input -> GameState -> GameState
makeMove input state = state

mainGameLoop :: GameState -> IO ()
mainGameLoop state = do
  putStrLn $ getGameStateStr state
  inputValue <- getInput
  print $ show inputValue

  mainGameLoop $ makeMove inputValue state

initialState :: GameState
initialState = replicate 4 $ replicate 4 0

main :: IO ()
main = do
  putStrLn "Welcome to 2048-hs"
  mainGameLoop initialState
  putStrLn "Thanks for playing"
