# 2048-hs

Another 2048 implementation in Haskell.

Writen for my Haskell course @ ZHAW.

## interesting bits
Some interesting bits from the code
### main game loop
The main game loop is implemented as a tail recursive function. It has 4 parts draw, prompt for Input, update state according to input and check game over

```Haskell
data GameState = GameState
  { grid :: Grid
  , score :: Integer
  }
{- ... -}
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
```

### getting the input:

The game is controllable by the normal WASD gaming keys as well as the arrow keys. parsing WASD was easy since I only needed to read one char with getChar.
The Arrow keys on the other hand where a bit more tricky since they are a escape sequence E.g. `\ESC[A` for arrow up. for them I needed to read in 3 consecutive chars and decide parse the last one.

```Haskell
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
```

### updating the board
The key idea is to avoid implementing the tile-shifting logic separately for all four directions. Instead, rotate the board so that every move becomes a left move, apply the left-shift logic once, and then rotate the board back.

```Haskell
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
  ...
```

### drawing
the main interface is implemented as a tui. and uses no libraries it just uses ANSI escape sequences for rendering the ui. [This](https://gist.github.com/ConnerWill/d4b6c776b509add763e17f9f113fd25b) gist for ANSI escape sequences was very helpful.
