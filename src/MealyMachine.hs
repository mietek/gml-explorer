{-

G.H. Mealy, “A method for synthesizing sequential circuits”, Bell System Technical Journal, 1955, pp. 1045–1079

-}

module MealyMachine where

import Data.Aeson (ToJSON)
import Data.ByteString (ByteString)
import Data.List (mapAccumL)
import Data.Maybe (catMaybes)
import Text.XML.Expat.SAX (SAXEvent)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Text.XML.Expat.SAX as X

import Toolkit


type Event =
    SAXEvent ByteString ByteString


type Transition =
    Event -> (MM, Maybe ByteString)


newtype MM = MM
  { stepMM :: Transition
  }


newMM :: Transition -> MM
newMM next =
    MM next


runMM :: MM -> L.ByteString -> [ByteString]
runMM mm =
    catMaybes . snd . mapAccumL stepMM mm . parse
  where
    parse =
        X.parse (X.ParseOptions Nothing Nothing)


await :: Transition -> (MM, Maybe ByteString)
await next =
    (MM next, Nothing)


yield :: (ToJSON a) => a -> Transition -> (MM, Maybe ByteString)
yield item next =
    (MM next, Just (encodeJSON item))


yieldStr :: ByteString -> Transition -> (MM, Maybe ByteString)
yieldStr item next =
    (MM next, Just item)
