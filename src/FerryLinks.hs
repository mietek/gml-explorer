module FerryLinks
  ( root
  ) where

import Data.Aeson ((.=), ToJSON, toJSON)
import Data.Text (Text)
import Text.XML.Expat.SAX (SAXEvent(..))
import qualified Data.Aeson as J

import Attributes
import MealyMachine


data FerryLink = FL
    { flTOID         :: Text
    , flNegativeNode :: Maybe Text
    , flPositiveNode :: Maybe Text
    }
  deriving (Eq, Ord, Show)


instance ToJSON FerryLink where
  toJSON FL{..} =
      J.object
        [ "toid"         .= flTOID
        , "negativeNode" .= flNegativeNode
        , "positiveNode" .= flPositiveNode
        ]


newFL :: Text -> FerryLink
newFL toid = FL
    { flTOID         = toid
    , flNegativeNode = Nothing
    , flPositiveNode = Nothing
    }


validFL :: FerryLink -> Bool
validFL FL{..} =
       flNegativeNode /= Nothing
    && flPositiveNode /= Nothing


root :: Transition
root (StartElement "osgb:networkMember" _) =
    await networkMember
root _ =
    await root


networkMember :: Transition
networkMember (EndElement "osgb:networkMember") =
    await root
networkMember (StartElement "osgb:FerryLink" attrs) =
    await (ferryLink (newFL (getTOID attrs)))
networkMember _ =
    await networkMember


ferryLink :: FerryLink -> Transition
ferryLink fl (EndElement "osgb:FerryLink")
  | validFL fl =
        yield fl networkMember
  | otherwise =
        error ("ferryLink: invalid osgb:FerryLink: " ++ show fl)
ferryLink fl (StartElement "osgb:directedNode" attrs) =
    case (flNegativeNode fl, flPositiveNode fl, getDirectedNode attrs) of
      (Nothing, _, Left nn) ->
        await (ferryLink (fl {flNegativeNode = Just nn}))
      (_, Nothing, Right pn) ->
        await (ferryLink (fl {flPositiveNode = Just pn}))
      _ ->
        error "ferryLink: expected 2 osgb:directedNode"
ferryLink fl _ =
    await (ferryLink fl)
