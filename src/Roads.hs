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
    { rTOID    :: Text
    , rGroup   :: Maybe Text
    , rTerm    :: Maybe Text
    , rName    :: Maybe Text
    , rMembers :: [Text]
    }
  deriving (Eq, Ord, Show)


instance ToJSON Road where
  toJSON R{..} =
      J.object $ strip
        [ "toid"    .= rTOID
        , "group"   .= rGroup
        , "term"    .= rTerm
        , "name"    .= rName
        , "members" .= rMembers
        ]
    where
      strip =
          filter (\(_, v) -> v /= J.Null)


newR :: Text -> Road
newR toid = R
    { rTOID    = toid
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


root :: Transition
root (StartElement "osgb:roadMember" _) =
    await roadMember
root _ =
    await root


roadMember :: Transition
roadMember (EndElement "osgb:roadMember") =
    await root
roadMember (StartElement "osgb:Road" attrs) =
    hold (newR (getTOID attrs)) road
roadMember _ =
    await roadMember


road :: Road -> Transition
road r (EndElement "osgb:Road")
  | validR r =
        yield r roadMember
  | otherwise =
        error ("road: invalid osgb:Road: " ++ show r)
road r (StartElement "osgb:descriptiveGroup" _)
  | rGroup r == Nothing =
        hold r (group none)
  | otherwise =
        error "road: expected 1 osgb:descriptiveGroup"
road r (StartElement "osgb:descriptiveTerm" _)
  | rTerm r == Nothing =
        hold r (term none)
  | otherwise =
        error "road: expected 1 osgb:descriptiveTerm"
road r (StartElement "osgb:roadName" _)
  | rName r == Nothing =
        hold r (name none)
  | otherwise =
        error "road: expected 1 osgb:roadName"
road r (StartElement "osgb:networkMember" attrs) =
    hold (r {rMembers = getHRef attrs : rMembers r}) road
road r _ =
    hold r road


group :: Builder -> Road -> Transition
group parts r (EndElement "osgb:descriptiveGroup") =
    hold (r {rGroup = Just (build parts)}) road
group parts r (CharacterData part) =
    hold r (group (parts <> part))
group parts r _ =
    hold r (group parts)


term :: Builder -> Road -> Transition
term parts r (EndElement "osgb:descriptiveTerm") =
    hold (r {rTerm = Just (build parts)}) road
term parts r (CharacterData part) =
    hold r (term (parts <> part))
term parts r _ =
    hold r (term parts)


name :: Builder -> Road -> Transition
name parts r (EndElement "osgb:roadName") =
    hold (r {rName = Just (build parts)}) road
name parts r (CharacterData part) =
    hold r (name (parts <> part))
name parts r _ =
    hold r (name parts)
