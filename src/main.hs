module Main where

import Control.Monad.IO.Class (MonadIO)
import Data.List (intercalate, transpose)
import System.IO
import System.Random (Random (..), getStdRandom)

type Grid = [[Integer]]

data GameState = GameState
  { grid :: Grid
  , score :: Integer
  }
  deriving (Show, Eq)

{-
 - #########################
 - ##### Render Logic ######
 - #########################
 - -}

cellSize :: Int
cellSize = 9

gridSize :: Int
gridSize = 4

padAround :: Int -> String -> String
padAround 0 str = str
padAround num str =
  padAround (num - 1) $
    if even (length str)
      then ' ' : str
      else str ++ " "

topRow :: [Integer] -> String
topRow row = intercalate "│" (map (\n -> " " ++ zeroOr n (replicate (cellSize - 2) ' ') (colorSwitch n backgroundEscapeRGB ++ "╔" ++ replicate (cellSize - 4) '═' ++ "╗" ++ resetEscape) ++ " ") row) ++ "\n"

bottomRow :: [Integer] -> String
bottomRow row = intercalate "│" (map (\n -> " " ++ zeroOr n (replicate (cellSize - 2) ' ') (colorSwitch n backgroundEscapeRGB ++ '╚' : replicate (cellSize - 4) '═' ++ "╝" ++ resetEscape) ++ " ") row) ++ "\n"

lineRow :: String
lineRow = intercalate "┼" (replicate 4 $ replicate cellSize '─') ++ "\n"

emptyRow :: String
emptyRow = intercalate "│" (replicate 4 $ replicate cellSize ' ') ++ "\n"

getGameStateStr :: GameState -> String
getGameStateStr state = "\ESCcScore: " ++ show (score state) ++ "\n" ++ getGridStr_ (grid state)
 where
  getGridStr_ :: Grid -> String
  getGridStr_ grid_ = intercalate lineRow $ map (\row -> emptyRow ++ topRow row ++ getRow_ row ++ "\n" ++ bottomRow row ++ emptyRow) grid_
   where
    getRow_ :: [Integer] -> String
    getRow_ row = intercalate "│" $ map (\i -> " " ++ zeroOr i " " (colorSwitch i backgroundEscapeRGB ++ "║") ++ padAround (cellSize - 4 - length (show i)) (zeroOr i " " (show i)) ++ zeroOr i " " ("║" ++ resetEscape) ++ " ") row

zeroOr :: Integer -> a -> a -> a
zeroOr 0 zeroV _ = zeroV
zeroOr _ _ nonZeroV = nonZeroV

forgroundEscapeRGB :: Integer -> Integer -> Integer -> String
forgroundEscapeRGB r g b = "\ESC[38;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"

backgroundEscapeRGB :: Integer -> Integer -> Integer -> String
backgroundEscapeRGB r g b = "\ESC[48;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"

resetEscape :: String
resetEscape = "\ESC[0m"

colorSwitch :: Integer -> (Integer -> Integer -> Integer -> String) -> String
colorSwitch num colorFunc = case num of
  2 -> colorFunc 0x64 0xB6 0xEE
  4 -> colorFunc 0x4C 0x9A 0xDD
  8 -> colorFunc 0x42 0x7B 0xD2
  16 -> colorFunc 0x33 0x64 0xC0
  32 -> colorFunc 0x2C 0x43 0xB0
  64 -> colorFunc 0x43 0x52 0xAB
  128 -> colorFunc 0xed 0xa8 0x1e
  256 -> colorFunc 0xEE 0xD2 0x68
  512 -> colorFunc 0xE8 0xBC 0x55
  1024 -> colorFunc 0xC8 0x9B 0x41
  2048 -> colorFunc 0xB4 0x84 0x3E
  _ -> colorFunc 0x56 0xAC 0xC3

{-
 - #########################
 - ##### Input Logic ######
 - #########################
 - -}

data Input = InUp | InLeft | InRight | InDown deriving (Show, Eq)

getInput :: IO Input
getInput = do
  putStr "in: "
  rawIn <- getChar
  val <- parseInput rawIn
  putStr "\n"

  maybe getInput return val
 where
  parseInput rawIn =
    if rawIn == '\ESC'
      -- arrow escape codes "\ESC[(A|D|B|C)"
      then do
        seccond <- getChar
        ( if seccond == '['
            then do
              dir <- getChar
              return
                ( case dir of
                    'A' -> Just InUp
                    'D' -> Just InLeft
                    'B' -> Just InDown
                    'C' -> Just InRight
                    _ -> Nothing
                )
            else return Nothing
          )
      else
        -- normal wasd
        return
          ( case rawIn of
              'w' -> Just InUp
              'd' -> Just InRight
              's' -> Just InDown
              'a' -> Just InLeft
              _ -> Nothing
          )

{-
 - #########################
 - ##### Update Logic ######
 - #########################
 - -}

update :: (MonadIO m) => Input -> GameState -> m GameState
update input startState = do
  let movedState = processInput input startState
  if movedState /= startState
    then do
      gridWithRandomPlacement <- placeNewRandomTile (grid movedState)
      return $ GameState gridWithRandomPlacement (score movedState)
    else
      return movedState

placeNewRandomTile :: (MonadIO m) => Grid -> m Grid
placeNewRandomTile currGrid = do
  (y, x) <- getRandomLoc currGrid
  newTileVal <- randRange 1 2
  return (mapi (\row rowi -> mapi (\val vali -> if x == vali && y == rowi then newTileVal * 2 else val) row) currGrid)
 where
  mapi :: (a -> Int -> a) -> [a] -> [a]
  mapi mapFn lst = mapi_ lst 0
   where
    mapi_ [] _ = []
    mapi_ (x : xs) i = mapFn x i : mapi_ xs (i + 1)

getRandomLoc :: (MonadIO m) => Grid -> m (Int, Int)
getRandomLoc state = do
  x <- randRange 0 3
  y <- randRange 0 3
  let val = gridGet state (x, y)
  if val == 0
    then return (x, y)
    else getRandomLoc state
 where
  gridGet :: Grid -> (Int, Int) -> Integer
  gridGet grid_ (x, y) = (grid_ !! x) !! y

randRange :: (MonadIO m, Random a) => a -> a -> m a
randRange lo hi = getStdRandom (randomR (lo, hi))

-- takes in a row and returns a tuple with the updated row and the addition to the score
rowMergers :: [Integer] -> ([Integer], Integer)
rowMergers row = rowMergersInner ([], row, 0)
 where
  rowMergersInner :: ([Integer], [Integer], Integer) -> ([Integer], Integer)
  rowMergersInner (rowAcc, restRow, scoreAcc) = case restRow of
    -- if the first 2 element in the remaining row are not zero and the same we merge them and add them to the back of the result accumulator & update the score accumulator
    x : y : xs | x /= 0 && x == y -> rowMergersInner (rowAcc ++ [x * 2], xs, scoreAcc + (2 * x))
    -- if we still have elements left we add them to the back of the accumulator - no score update
    x : xs -> rowMergersInner (rowAcc ++ [x], xs, scoreAcc)
    -- recursive end condition no chars left to process in the row
    -- fill up accumulator with zeros since they potentially got destroyed by mergers
    [] -> (rowAcc ++ replicate (gridSize - length rowAcc) 0, scoreAcc)

moveRow :: [Integer] -> [Integer]
moveRow row = case row of
  x : xs
    | x == 0 -> moveRow xs ++ [x] -- move leading zero to back
    | otherwise -> x : moveRow xs -- keep x at current position
  [] -> []

processInput :: Input -> GameState -> GameState
processInput input state = do
  let rotatedGrid = rotateGrid input (grid state)
  let movedGrid = map moveRow rotatedGrid

  let (mergedGrid, scores) = unzip $ map rowMergers movedGrid
  GameState (rotateGridBack input mergedGrid) (score state + sum scores)
 where
  rotateGrid :: Input -> Grid -> Grid
  rotateGrid input_ grid_ = case input_ of
    InRight -> map reverse grid_
    InLeft -> grid_
    InUp -> transpose grid_
    InDown -> map reverse $ transpose grid_

  rotateGridBack :: Input -> Grid -> Grid
  rotateGridBack input_ grid_ = case input_ of
    InRight -> map reverse grid_
    InLeft -> grid_
    InUp -> transpose grid_
    InDown -> transpose $ map reverse grid_

{-
 - #########################
 - #### Main Game Logic ####
 - #########################
 - -}

mainGameLoop :: GameState -> IO ()
mainGameLoop state = do
  draw state
  inputValue <- getInput
  nextState <- update inputValue state

  if isGameOver nextState
    then do
      draw nextState
      putStrLn "game over"
    else mainGameLoop nextState
 where
  draw :: GameState -> IO ()
  draw state_ = do
    putStrLn $ getGameStateStr state_

  isGameOver :: GameState -> Bool
  isGameOver state_ =
    (state_ == processInput InDown state_)
      && (state_ == processInput InRight state_)
      && (state_ == processInput InLeft state_)
      && (state_ == processInput InUp state_)

initialState :: (MonadIO m) => m GameState
initialState = do
  let initalGrid = replicate gridSize $ replicate gridSize 0
  firstPlacement <- placeNewRandomTile initalGrid
  seccondPlacement <- placeNewRandomTile firstPlacement
  return $ GameState seccondPlacement 0

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to 2048-hs"
  initialState >>= mainGameLoop
  putStrLn "Thanks for playing"
