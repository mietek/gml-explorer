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
    await (ferryNode (newRN (getTOID attrs)))
networkMember _ =
    await networkMember


ferryNode :: FerryNode -> Transition
ferryNode fn (EndElement "osgb:FerryNode")
  | validRN fn =
        yield fn networkMember
ferryNode fn (StartElement "osgb:point" _)
  | fnPoint fn == Nothing =
        await (osgbPoint fn)
  | otherwise =
        error "ferryNode: expected 1 osgb:point"
ferryNode fn _ =
    await (ferryNode fn)


osgbPoint :: FerryNode -> Transition
osgbPoint fn (EndElement "osgb:point") =
    await (ferryNode fn)
osgbPoint fn (StartElement "gml:Point" _)
  | fnPoint fn == Nothing =
        await (gmlPoint fn)
  | otherwise =
        error "osgbPoint: expected 1 gml:Point"
osgbPoint fn _ =
    await (osgbPoint fn)


gmlPoint :: FerryNode -> Transition
gmlPoint fn (EndElement "gml:Point") =
    await (osgbPoint fn)
gmlPoint fn (StartElement "gml:coordinates" _)
  | fnPoint fn == Nothing =
        await (coordinates none fn)
  | otherwise =
        error "gmlPoint: expected 1 gml:coordinates"
gmlPoint fn _ =
    await (gmlPoint fn)


coordinates :: Builder -> FerryNode -> Transition
coordinates parts fn (EndElement "gml:coordinates") =
    await (gmlPoint fn {fnPoint = Just (decodePoint (build parts))})
coordinates parts fn (CharacterData part) =
    await (coordinates (parts <> part) fn)
coordinates parts fn _ =
    await (coordinates parts fn)
