module State (
  -- * State
  State (..),
  defState,
  toggleClipboard,

  -- * Grid
  Grid,
  MInt,
  buildMInt,

  -- * Manipulating the cursor
  Direction2D (..),
  Direction1D (..),
  move,
  del,
  fill,
  toggleNeg,

  -- * Converting to other representations
  handleConvertEvent,
) where

import Util

import Brick (BrickEvent (VtyEvent), EventM, Next, halt)
import Data.Coerce (coerce)
import Data.List (transpose, find)
import Graphics.Vty (Event (EvKey), Key (..))


-- | An entry in the matrix; i.e., a "matrix integer".
newtype MInt = MInt String
  deriving newtype (Semigroup)

instance Show MInt where
  show :: MInt -> String
  show = coerce             -- 'show' for 'String', but without quotes around it.

-- | Try to construct an 'MInt' from a single character.
buildMInt :: Char -> Maybe MInt
buildMInt c
  | c `elem` ("0123456789" :: String) = Just $ MInt [c]
  | otherwise                         = Nothing

-- | Interpret an 'MInt' as a string and apply a function to it.
asString :: (String -> String) -> MInt -> MInt
asString f = coerce . f . coerce

-- | Our grid of numbers.
type Grid = [[MInt]]

data State = State
  { size  :: (Int, Int)   -- ^ Size of the matrix in total
  , pos   :: (Int, Int)   -- ^ Current cursor position
  , grid  :: Grid         -- ^ *Transposed* entries
  , convs :: [Convert]    -- ^ Available conversions
  , res   :: String       -- ^ Result to return
  , useClipboard :: Bool  -- ^ Copy result to clipboard?
  } deriving stock (Show)

gridL :: Lens' State Grid
gridL = lens grid (\s g -> s{ grid = g })

resL :: Lens' State String
resL = lens res (\s g -> s{ res = g })

-- | Starting state is the zero matrix.
defState :: Int    -- ^ Number of rows
         -> Int    -- ^ Number of columns
         -> State
defState r c = State
  { size  = (r, c)
  , pos   = (0, 0)
  , grid  = replicate c (replicate r (MInt "0"))
  , convs = conversions
  , res   = ""
  , useClipboard = True
  }

-- | Toggle whether to copy the end result into the clipboard.
toggleClipboard :: State -> State
toggleClipboard s = s{ useClipboard = not $ useClipboard s }

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
  kill :: String -> String
  kill = case d of
    Front -> tail
    Back  -> init

-- | Fill in a number at a certain position.
fill :: MInt -> State -> State
fill n s = modifyPoint s (<> coerce n)

-- | Toggle a number to be negative.
toggleNeg :: State -> State
toggleNeg s = modifyPoint s $ \case
  '-' : ss -> ss
  ss       -> '-' : ss

clean :: String -> String
clean = \case
  '-' : s -> '-' : orZero (dropWhile (== '0') s)
  s       ->       orZero $ dropWhile (== '0') s
 where
   orZero :: String -> String
   orZero n = if null n then "0" else n

modifyPoint :: State -> (String -> String) -> State
modifyPoint s@State{ pos = (r, c) } f =
  s & gridL . ix r . ix c %~ asString (clean . f)

-----------------------------------------------------------------------
-- Conversions

data Convert = Convert
  { cKey  :: Key
  , cLang :: String
  , cCon  :: State -> State
  }

instance Show Convert where
  show :: Convert -> String
  show Convert{ cKey, cLang } =
    (<> ": " <> cLang) $ case cKey of
                           KChar c -> [c]
                           k       -> show k

conversions :: [Convert]
conversions = [toClojureArr, toHaskellLists, toPythonMat]

handleConvertEvent :: State -> BrickEvent () e -> Maybe (EventM () (Next State))
handleConvertEvent s@State{ convs } = \case
  VtyEvent ve -> case ve of
    EvKey c [] -> case find ((== c) . cKey) convs of
      Just Convert{ cCon } -> Just $ halt (cCon s)
      _ -> Nothing
    _ -> Nothing
  _ -> Nothing

toHaskellLists :: Convert
toHaskellLists = Convert
  { cKey  = KChar 'h'
  , cLang = "Haskell"
  , cCon  = \s -> s & resL .~ showGrid s
  }

toPythonMat :: Convert
toPythonMat = Convert
  { cKey  = KChar 'p'
  , cLang = "Python"
  , cCon  = \s -> s & resL .~ "Matrix(" <> showGrid s <> ")"
  }

toClojureArr :: Convert
toClojureArr = Convert
  { cKey  = KChar 'c'
  , cLang = "Clojure"
  , cCon  = \s -> s & resL .~ replace "," " " (showGrid s)
  }

showGrid :: State -> String
showGrid = show . transpose . grid
