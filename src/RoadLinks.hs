module RoadLinks
  ( root
  ) where

import Data.Aeson ((.=), ToJSON, toJSON)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Text.XML.Expat.SAX (SAXEvent(..))
import qualified Data.Aeson as J

import Attributes
import MealyMachine
import Toolkit


data RoadLink = RL
    { rlTOID         :: Text
    , rlTerm         :: Maybe Text
    , rlNature       :: Maybe Text
    , rlPolyline     :: Maybe [Double]
    , rlNegativeNode :: Maybe Text
    , rlPositiveNode :: Maybe Text
    }
  deriving (Eq, Ord, Show)


instance ToJSON RoadLink where
  toJSON RL{..} =
      J.object
        [ "toid"         .= rlTOID
        , "term"         .= rlTerm
        , "nature"       .= rlNature
        , "polyline"     .= rlPolyline
        , "negativeNode" .= rlNegativeNode
        , "positiveNode" .= rlPositiveNode
        ]


newRL :: Text -> RoadLink
newRL toid = RL
    { rlTOID         = toid
    , rlTerm         = Nothing
    , rlNature       = Nothing
    , rlPolyline     = Nothing
    , rlNegativeNode = Nothing
    , rlPositiveNode = Nothing
    }


validRL :: RoadLink -> Bool
validRL RL{..} =
       rlTerm         /= Nothing
    && rlNature       /= Nothing
    && rlPolyline     /= Nothing && l >= 4 && l `mod` 2 == 0
    && rlNegativeNode /= Nothing
    && rlPositiveNode /= Nothing
  where
    l = length (fromJust rlPolyline)


root :: Transition
root (StartElement "osgb:networkMember" _) =
    await networkMember
root _ =
    await root


networkMember :: Transition
networkMember (EndElement "osgb:networkMember") =
    await root
networkMember (StartElement "osgb:RoadLink" attrs) =
    hold (newRL (getTOID attrs)) roadLink
networkMember _ =
    await networkMember


roadLink :: RoadLink -> Transition
roadLink rl (EndElement "osgb:RoadLink")
  | validRL rl =
        yield rl networkMember
  | otherwise =
        error ("roadLink: invalid osgb:RoadLink: " ++ show rl)
roadLink rl (StartElement "osgb:descriptiveTerm" _)
  | rlTerm rl == Nothing =
        hold rl (term none)
  | otherwise =
        error "roadLink: expected 1 osgb:descriptiveTerm"
roadLink rl (StartElement "osgb:natureOfRoad" _)
  | rlNature rl == Nothing =
        hold rl (nature none)
  | otherwise =
        error "roadLink: expected 1 osgb:natureOfRoad"
roadLink rl (StartElement "osgb:polyline" _)
  | rlPolyline rl == Nothing =
        hold rl polyline
  | otherwise =
        error "roadLink: expected 1 osgb:polyline"
roadLink rl (StartElement "osgb:directedNode" attrs) =
    case (rlNegativeNode rl, rlPositiveNode rl, getDirectedNode attrs) of
      (Nothing, _, Left nn) ->
        hold (rl {rlNegativeNode = Just nn}) roadLink
      (_, Nothing, Right pn) ->
        hold (rl {rlPositiveNode = Just pn}) roadLink
      _ ->
        error "roadLink: expected 2 osgb:directedNode"
roadLink rl _ =
    hold rl roadLink


term :: Builder -> RoadLink -> Transition
term parts rl (EndElement "osgb:descriptiveTerm") =
    hold (rl {rlTerm = Just (build parts)}) roadLink
term parts rl (CharacterData part) =
    hold rl (term (parts <> part))
term parts rl _ =
    hold rl (term parts)


nature :: Builder -> RoadLink -> Transition
nature parts rl (EndElement "osgb:natureOfRoad") =
    hold (rl {rlNature = Just (build parts)}) roadLink
nature parts rl (CharacterData part) =
    hold rl (nature (parts <> part))
nature parts rl _ =
    hold rl (nature parts)


polyline :: RoadLink -> Transition
polyline rl (EndElement "osgb:polyline") =
    hold rl roadLink
polyline rl (StartElement "gml:LineString" _)
  | rlPolyline rl == Nothing =
        hold rl lineString
  | otherwise =
        error "polyline: expected 1 gml:LineString"
polyline rl _ =
    hold rl polyline


lineString :: RoadLink -> Transition
lineString rl (EndElement "gml:LineString") =
    hold rl polyline
lineString rl (StartElement "gml:coordinates" _)
  | rlPolyline rl == Nothing =
        hold rl (coordinates none)
  | otherwise =
        error "lineString: expected 1 gml:coordinates"
lineString rl _ =
    hold rl lineString


coordinates :: Builder -> RoadLink -> Transition
coordinates parts rl (EndElement "gml:coordinates") =
    hold (rl {rlPolyline = Just (decodePolyline (build parts))}) lineString
coordinates parts rl (CharacterData part) =
    hold rl (coordinates (parts <> part))
coordinates parts rl _ =
    hold rl (coordinates parts)
