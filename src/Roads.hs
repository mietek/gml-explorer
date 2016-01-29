module Roads
  ( root
  ) where

import Data.Aeson ((.=), ToJSON, toJSON)
import Data.Text (Text)
import Text.XML.Expat.SAX (SAXEvent(..))
import qualified Data.Aeson as J

import Attributes
import MealyMachine
import Toolkit


data Road = R
    { rIndex   :: Int
    , rTOID    :: Text
    , rGroup   :: Maybe Text
    , rTerm    :: Maybe Text
    , rName    :: Maybe Text
    , rMembers :: [Text]
    }
  deriving (Eq, Ord, Show)


instance ToJSON Road where
  toJSON R{..} =
      J.object $ strip
        [ "index"   .= rIndex
        , "toid"    .= rTOID
        , "group"   .= rGroup
        , "term"    .= rTerm
        , "name"    .= rName
        , "members" .= rMembers
        ]
    where
      strip =
          filter (\(_, v) -> v /= J.Null)


newR :: Int -> Text -> Road
newR index toid = R
    { rIndex   = index
    , rTOID    = toid
    , rGroup   = Nothing
    , rTerm    = Nothing
    , rName    = Nothing
    , rMembers = []
    }


validR :: Road -> Bool
validR R{..} =
       rGroup   /= Nothing
    && rName    /= Nothing
    && rMembers /= []


root :: Int -> Transition
root index (StartElement "osgb:roadMember" _) =
    await (roadMember index)
root index _ =
    await (root index)


roadMember :: Int -> Transition
roadMember index (EndElement "osgb:roadMember") =
    await (root index)
roadMember index (StartElement "osgb:Road" attrs) =
    await (road (newR index (getTOID attrs)))
roadMember index _ =
    await (roadMember index)


road :: Road -> Transition
road r (EndElement "osgb:Road")
  | validR r =
        yield r (roadMember (rIndex r + 1))
  | otherwise =
        error ("road: invalid osgb:Road: " ++ show r)
road r (StartElement "osgb:descriptiveGroup" _)
  | rGroup r == Nothing =
        await (group none r)
  | otherwise =
        error "road: expected 1 osgb:descriptiveGroup"
road r (StartElement "osgb:descriptiveTerm" _)
  | rTerm r == Nothing =
        await (term none r)
  | otherwise =
        error "road: expected 1 osgb:descriptiveTerm"
road r (StartElement "osgb:roadName" _)
  | rName r == Nothing =
        await (name none r)
  | otherwise =
        error "road: expected 1 osgb:roadName"
road r (StartElement "osgb:networkMember" attrs) =
    await (road r {rMembers = getHRef attrs : rMembers r})
road r _ =
    await (road r)


group :: Builder -> Road -> Transition
group parts r (EndElement "osgb:descriptiveGroup") =
    await (road r {rGroup = Just (build parts)})
group parts r (CharacterData part) =
    await (group (parts <> part) r)
group parts r _ =
    await (group parts r)


term :: Builder -> Road -> Transition
term parts r (EndElement "osgb:descriptiveTerm") =
    await (road r {rTerm = Just (build parts)})
term parts r (CharacterData part) =
    await (term (parts <> part) r)
term parts r _ =
    await (term parts r)


name :: Builder -> Road -> Transition
name parts r (EndElement "osgb:roadName") =
    await (road r {rName = Just (build parts)})
name parts r (CharacterData part) =
    await (name (parts <> part) r)
name parts r _ =
    await (name parts r)
