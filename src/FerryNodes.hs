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
    { fnIndex :: Int
    , fnTOID  :: Text
    , fnPoint :: Maybe [Double]
    }
  deriving (Eq, Ord, Show)


instance ToJSON FerryNode where
  toJSON RN{..} =
      J.object
        [ "index" .= fnIndex
        , "toid"  .= fnTOID
        , "point" .= fnPoint
        ]


newRN :: Int -> Text -> FerryNode
newRN index toid = RN
    { fnIndex = index
    , fnTOID  = toid
    , fnPoint = Nothing
    }


validRN :: FerryNode -> Bool
validRN RN{..} =
    fnPoint /= Nothing && l == 2
  where
    l = length (fromJust fnPoint)


root :: Int -> Transition
root index (StartElement "osgb:networkMember" _) =
    await (networkMember index)
root index _ =
    await (root index)


networkMember :: Int -> Transition
networkMember index (EndElement "osgb:networkMember") =
    await (root index)
networkMember index (StartElement "osgb:FerryNode" attrs) =
    await (ferryNode (newRN index (getTOID attrs)))
networkMember index _ =
    await (networkMember index)


ferryNode :: FerryNode -> Transition
ferryNode fn (EndElement "osgb:FerryNode")
  | validRN fn =
        yield fn (networkMember (fnIndex fn + 1))
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
