module State (
  -- * State
  State (..),
  Grid,
  defState,
  -- * Manipulating the cursor
  Direction2D (..),
  Direction1D (..),
  move,
  del,
  fill,
) where

import Util


type Grid = [[String]]

data State = State
  { size :: (Int, Int)  -- ^ Size of the matrix in total
  , pos  :: (Int, Int)  -- ^ Current cursor position
  , grid :: Grid        -- ^ Entries
  } deriving stock (Show)

gridL :: Lens State State Grid Grid
gridL = lens grid (\s g -> s{ grid = g })

-- | Starting state is the zero matrix.
defState :: State
defState = State
  { size = (3, 3)                          -- TODO: CLI args or separate TUI window for size
  , pos  = (0, 0)
  , grid = replicate 3 (replicate 3 "0")
  }

-- | Directions one can move the cursor in
data Direction2D = North | East | South | West

-- | Move the cursor in the given direction; wraps around if necessary.
move :: Direction2D -> State -> State
move d s@State{ pos = (pr, pc), size = (r, c) } = s{ pos = go }
 where
  go :: (Int, Int)
  go = wrap $ case d of
    East  -> (pr + 1, pc    )
    South -> (pr    , pc + 1)
    West  -> (pr - 1, pc    )
    North -> (pr    , pc - 1)

  wrap :: (Int, Int) -> (Int, Int)
  wrap (ew, ns) = (ew `mod` r, ns `mod` c)

-- | Places one can delete things from.
data Direction1D = Front | Back

-- | Delete a character the specified direction direction.
del :: Direction1D -> State -> State
del d s = modifyPoint s kill
 where
  kill = case d of
    Front -> dropWhile (== '0') . tail
    Back  -> init

-- | Fill in a number at a certain position.
fill :: Char -> State -> State
fill n s = modifyPoint s (dropWhile (== '0') . (<> [n]))

orZero :: String -> String
orZero num = if null num then "0" else num

modifyPoint :: State -> (String -> String) -> State
modifyPoint s@State{ pos = (r, c) } f = s & gridL . ix r . ix c %~ orZero . f
