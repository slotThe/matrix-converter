module Util (
  module Exports,
  imap,
) where

import Control.Monad as Exports
import Lens.Micro    as Exports

-- | Like 'map', but indexed.
imap :: (Int -> a -> b) -> [a] -> [b]
imap f = zipWith f [0 ..]
