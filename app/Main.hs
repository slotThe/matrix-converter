module Main (main) where

import State
import TUI
import Util

import Brick (defaultMain)
import System.Clipboard (setClipboardString)
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  (rows, cols) <- getArgs >>= \case
    [rc]   -> pure . fromMaybe (3, 3) $ both readMaybe (rc, rc)
    [r, c] -> pure . fromMaybe (3, 3) $ both readMaybe (r , c)
    _      -> errorWithoutStackTrace "Please insert the number of rows \
                                     \and columns as positional arguments."
  s <- defaultMain app (defState rows cols)
  when (useClipboard s) $
    setClipboardString (res s)
  putStrLn (res s)
