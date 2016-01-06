module RoadNodes
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


data RoadNode = RN
    { rnTOID  :: Text
    , rnPoint :: Maybe [Double]
    }
  deriving (Eq, Ord, Show)


instance ToJSON RoadNode where
  toJSON RN{..} =
      J.object
        [ "toid"  .= rnTOID
        , "point" .= rnPoint
        ]


newRN :: Text -> RoadNode
newRN toid = RN
    { rnTOID  = toid
    , rnPoint = Nothing
    }


validRN :: RoadNode -> Bool
validRN RN{..} =
    rnPoint /= Nothing && l == 2
  where
    l = length (fromJust rnPoint)


root :: Transition
root (StartElement "osgb:networkMember" _) =
    await networkMember
root _ =
    await root


networkMember :: Transition
networkMember (EndElement "osgb:networkMember") =
    await root
networkMember (StartElement "osgb:RoadNode" attrs) =
    hold (newRN (getTOID attrs)) roadNode
networkMember _ =
    await networkMember


roadNode :: RoadNode -> Transition
roadNode rn (EndElement "osgb:RoadNode")
  | validRN rn =
        yield rn networkMember
roadNode rn (StartElement "osgb:point" _)
  | rnPoint rn == Nothing =
        hold rn osgbPoint
  | otherwise =
        error "roadNode: expected 1 osgb:point"
roadNode rn _ =
    hold rn roadNode


osgbPoint :: RoadNode -> Transition
osgbPoint rn (EndElement "osgb:point") =
    hold rn roadNode
osgbPoint rn (StartElement "gml:Point" _)
  | rnPoint rn == Nothing =
        hold rn gmlPoint
  | otherwise =
        error "osgbPoint: expected 1 gml:Point"
osgbPoint rn _ =
    hold rn osgbPoint


gmlPoint :: RoadNode -> Transition
gmlPoint rn (EndElement "gml:Point") =
    hold rn osgbPoint
gmlPoint rn (StartElement "gml:coordinates" _)
  | rnPoint rn == Nothing =
        hold rn (coordinates none)
  | otherwise =
        error "gmlPoint: expected 1 gml:coordinates"
gmlPoint rn _ =
    hold rn gmlPoint


coordinates :: Builder -> RoadNode -> Transition
coordinates parts rn (EndElement "gml:coordinates") =
    hold (rn {rnPoint = Just (decodePoint (build parts))}) gmlPoint
coordinates parts rn (CharacterData part) =
    hold rn (coordinates (parts <> part))
coordinates parts rn _ =
    hold rn (coordinates parts)
