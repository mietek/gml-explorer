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
    await (roadNode (newRN (getTOID attrs)))
networkMember _ =
    await networkMember


roadNode :: RoadNode -> Transition
roadNode rn (EndElement "osgb:RoadNode")
  | validRN rn =
        yield rn networkMember
roadNode rn (StartElement "osgb:point" _)
  | rnPoint rn == Nothing =
        await (osgbPoint rn)
  | otherwise =
        error "roadNode: expected 1 osgb:point"
roadNode rn _ =
    await (roadNode rn)


osgbPoint :: RoadNode -> Transition
osgbPoint rn (EndElement "osgb:point") =
    await (roadNode rn)
osgbPoint rn (StartElement "gml:Point" _)
  | rnPoint rn == Nothing =
        await (gmlPoint rn)
  | otherwise =
        error "osgbPoint: expected 1 gml:Point"
osgbPoint rn _ =
    await (osgbPoint rn)


gmlPoint :: RoadNode -> Transition
gmlPoint rn (EndElement "gml:Point") =
    await (osgbPoint rn)
gmlPoint rn (StartElement "gml:coordinates" _)
  | rnPoint rn == Nothing =
        await (coordinates none rn)
  | otherwise =
        error "gmlPoint: expected 1 gml:coordinates"
gmlPoint rn _ =
    await (gmlPoint rn)


coordinates :: Builder -> RoadNode -> Transition
coordinates parts rn (EndElement "gml:coordinates") =
    await (gmlPoint rn {rnPoint = Just (decodePoint (build parts))})
coordinates parts rn (CharacterData part) =
    await (coordinates (parts <> part) rn)
coordinates parts rn _ =
    await (coordinates parts rn)
