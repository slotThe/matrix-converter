module Main (main) where

import State
import TUI
import Util

import Brick (defaultMain)

main :: IO ()
main = void $ defaultMain app defState
