module State (
  State (..),
  Grid,
  defState,
  fill,
  del,
  move,
  Direction2D (..),
  Direction1D (..),
) where

import Util


type Grid = [[String]]

data State = State
  { size :: (Int, Int)
  , pos  :: (Int, Int)
  , grid :: Grid
  } deriving stock (Show)

gridL :: Lens State State Grid Grid
gridL = lens grid (\s g -> s{ grid = g })

defState :: State
defState = State
  { size = (3, 3)                          -- TODO: CLI args or separate TUI window for size
  , pos  = (0, 0)
  , grid = replicate 3 (replicate 3 "0")
  }

data Direction2D = North | East | South | West
data Direction1D = Front | Back

move :: Direction2D -> State -> State
move d s@State{ pos = (pr, pc), size = (r, c) } = s{ pos = go }
 where
  go :: (Int, Int)
  go = wrap case d of
    East  -> (pr + 1, pc    )
    South -> (pr    , pc + 1)
    West  -> (pr - 1, pc    )
    North -> (pr    , pc - 1)

  wrap :: (Int, Int) -> (Int, Int)
  wrap (ew, ns) = (ew `mod` r, ns `mod` c)

fill :: Integer -> State -> State
fill n s = modifyPoint s (dropWhile (== '0') . (<> show n))

del :: Direction1D -> State -> State
del d s = modifyPoint s kill
 where
  kill = case d of
    Front -> dropWhile (== '0') . tail
    Back  -> init

orZero :: String -> String
orZero num = if null num then "0" else num

modifyPoint :: State -> (String -> String) -> State
modifyPoint s@State{ pos = (r, c) } f = s & gridL . ix r . ix c %~ orZero . f
