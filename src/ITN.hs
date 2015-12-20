{-# LANGUAGE OverloadedStrings #-}

module ITN where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)

import MealyMachine


yieldTOID :: [(ByteString, ByteString)] -> Transition -> (MealyMachine, Maybe ByteString)
yieldTOID attrs next =
    let toid = fromJust (lookup "fid" attrs)
    in  yield (toid `B.snoc` ' ') next


yieldDirectedNode :: [(ByteString, ByteString)] -> Transition -> (MealyMachine, Maybe ByteString)
yieldDirectedNode attrs next =
    let orientation = fromJust (lookup "orientation" attrs)
        hashToid    = fromJust (lookup "xlink:href" attrs)
    in  yield (orientation `B.append` B.tail hashToid `B.snoc` ' ') next
