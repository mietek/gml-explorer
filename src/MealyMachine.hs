{-

G.H. Mealy, “A method for synthesizing sequential circuits”, Bell System Technical Journal, 1955, pp. 1045–1079

-}

module MealyMachine where

import Data.ByteString (ByteString)
import Text.XML.Expat.SAX (SAXEvent)


type Event = SAXEvent ByteString ByteString

type Transition = Event -> (MealyMachine, Maybe ByteString)

newtype MealyMachine = MM
  { runMealyMachine :: Transition
  }


newMealyMachine :: Transition -> MealyMachine
newMealyMachine next = MM next

await :: Transition -> (MealyMachine, Maybe ByteString)
await next = (MM next, Nothing)

yield :: ByteString -> Transition -> (MealyMachine, Maybe ByteString)
yield item next = (MM next, Just item)
