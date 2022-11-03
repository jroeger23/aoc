module AoC2021.Day09 where

import           Data.Functor ((<&>))
import qualified Data.Matrix  as M


genericConv2 :: Int -> Int -> (M.Matrix (Maybe a) -> b) ->  M.Matrix a -> M.Matrix b
genericConv2 kh kw kernel m = undefined
  where
    mh = M.nrows m
    mw = M.ncols m
    rRanges =  [0 .. mh-kh] <&> \i -> (1+i, kh+i)
