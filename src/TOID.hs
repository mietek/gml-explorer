{-# LANGUAGE OverloadedStrings #-}

module TOID where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)

import MealyMachine


yieldTOID :: [(ByteString, ByteString)] -> Transition -> (MealyMachine, Maybe ByteString)
yieldTOID attrs next =
    let toid = fromJust (lookup "fid" attrs)
    in  yield (B.snoc toid ' ') next
