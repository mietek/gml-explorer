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
    await (roadLink (newRL (getTOID attrs)))
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
        await (term none rl)
  | otherwise =
        error "roadLink: expected 1 osgb:descriptiveTerm"
roadLink rl (StartElement "osgb:natureOfRoad" _)
  | rlNature rl == Nothing =
        await (nature none rl)
  | otherwise =
        error "roadLink: expected 1 osgb:natureOfRoad"
roadLink rl (StartElement "osgb:polyline" _)
  | rlPolyline rl == Nothing =
        await (polyline rl)
  | otherwise =
        error "roadLink: expected 1 osgb:polyline"
roadLink rl (StartElement "osgb:directedNode" attrs) =
    case (rlNegativeNode rl, rlPositiveNode rl, getDirectedNode attrs) of
      (Nothing, _, Left nn) ->
        await (roadLink rl {rlNegativeNode = Just nn})
      (_, Nothing, Right pn) ->
        await (roadLink rl {rlPositiveNode = Just pn})
      _ ->
        error "roadLink: expected 2 osgb:directedNode"
roadLink rl _ =
    await (roadLink rl)


term :: Builder -> RoadLink -> Transition
term parts rl (EndElement "osgb:descriptiveTerm") =
    await (roadLink rl {rlTerm = Just (build parts)})
term parts rl (CharacterData part) =
    await (term (parts <> part) rl)
term parts rl _ =
    await (term parts rl)


nature :: Builder -> RoadLink -> Transition
nature parts rl (EndElement "osgb:natureOfRoad") =
    await (roadLink rl {rlNature = Just (build parts)})
nature parts rl (CharacterData part) =
    await (nature (parts <> part) rl)
nature parts rl _ =
    await (nature parts rl)


polyline :: RoadLink -> Transition
polyline rl (EndElement "osgb:polyline") =
    await (roadLink rl)
polyline rl (StartElement "gml:LineString" _)
  | rlPolyline rl == Nothing =
        await (lineString rl)
  | otherwise =
        error "polyline: expected 1 gml:LineString"
polyline rl _ =
    await (polyline rl)


lineString :: RoadLink -> Transition
lineString rl (EndElement "gml:LineString") =
    await (polyline rl)
lineString rl (StartElement "gml:coordinates" _)
  | rlPolyline rl == Nothing =
        await (coordinates none rl)
  | otherwise =
        error "lineString: expected 1 gml:coordinates"
lineString rl _ =
    await (lineString rl)


coordinates :: Builder -> RoadLink -> Transition
coordinates parts rl (EndElement "gml:coordinates") =
    await (lineString rl {rlPolyline = Just (decodePolyline (build parts))})
coordinates parts rl (CharacterData part) =
    await (coordinates (parts <> part) rl)
coordinates parts rl _ =
    await (coordinates parts rl)
