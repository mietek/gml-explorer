module FerryNodes
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


data FerryNode = RN
    { fnTOID  :: Text
    , fnPoint :: Maybe [Double]
    }
  deriving (Eq, Ord, Show)


instance ToJSON FerryNode where
  toJSON RN{..} =
      J.object
        [ "toid"  .= fnTOID
        , "point" .= fnPoint
        ]


newRN :: Text -> FerryNode
newRN toid = RN
    { fnTOID  = toid
    , fnPoint = Nothing
    }


validRN :: FerryNode -> Bool
validRN RN{..} =
    fnPoint /= Nothing && l == 2
  where
    l = length (fromJust fnPoint)


root :: Transition
root (StartElement "osgb:networkMember" _) =
    await networkMember
root _ =
    await root


networkMember :: Transition
networkMember (EndElement "osgb:networkMember") =
    await root
networkMember (StartElement "osgb:FerryNode" attrs) =
    hold (newRN (getTOID attrs)) ferryNode
networkMember _ =
    await networkMember


ferryNode :: FerryNode -> Transition
ferryNode fn (EndElement "osgb:FerryNode")
  | validRN fn =
        yield fn networkMember
ferryNode fn (StartElement "osgb:point" _)
  | fnPoint fn == Nothing =
        hold fn osgbPoint
  | otherwise =
        error "ferryNode: expected 1 osgb:point"
ferryNode fn _ =
    hold fn ferryNode


osgbPoint :: FerryNode -> Transition
osgbPoint fn (EndElement "osgb:point") =
    hold fn ferryNode
osgbPoint fn (StartElement "gml:Point" _)
  | fnPoint fn == Nothing =
        hold fn gmlPoint
  | otherwise =
        error "osgbPoint: expected 1 gml:Point"
osgbPoint fn _ =
    hold fn osgbPoint


gmlPoint :: FerryNode -> Transition
gmlPoint fn (EndElement "gml:Point") =
    hold fn osgbPoint
gmlPoint fn (StartElement "gml:coordinates" _)
  | fnPoint fn == Nothing =
        hold fn (coordinates none)
  | otherwise =
        error "gmlPoint: expected 1 gml:coordinates"
gmlPoint fn _ =
    hold fn gmlPoint


coordinates :: Builder -> FerryNode -> Transition
coordinates parts fn (EndElement "gml:coordinates") =
    hold (fn {fnPoint = Just (decodePoint (build parts))}) gmlPoint
coordinates parts fn (CharacterData part) =
    hold fn (coordinates (parts <> part))
coordinates parts fn _ =
    hold fn (coordinates parts)
