{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Util (
  module Exports,
  imap,
  replace,
) where

import Control.Monad as Exports
import Data.Bool     as Exports
import Data.Maybe    as Exports
import Lens.Micro    as Exports

import Data.List (stripPrefix)
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)

-- | Like 'map', but indexed.
imap :: (Int -> a -> b) -> [a] -> [b]
imap f = zipWith f [0 ..]

replace :: forall a. Eq a => [a] -> [a] -> [a] -> [a]
replace []   _  as = as
replace from to as = go as
 where
  go :: [a] -> [a]
  go xs
    | Just ys        <- stripPrefix from xs = to <> go ys
    | Just (x :| ys) <- nonEmpty xs         = x : go ys
    | otherwise                             = []
{-# INLINE replace #-}
