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
    { flIndex        :: Int
    , flTOID         :: Text
    , flNegativeNode :: Maybe Text
    , flPositiveNode :: Maybe Text
    }
  deriving (Eq, Ord, Show)


instance ToJSON FerryLink where
  toJSON FL{..} =
      J.object
        [ "index"        .= flIndex
        , "toid"         .= flTOID
        , "negativeNode" .= flNegativeNode
        , "positiveNode" .= flPositiveNode
        ]


newFL :: Int -> Text -> FerryLink
newFL index toid = FL
    { flIndex        = index
    , flTOID         = toid
    , flNegativeNode = Nothing
    , flPositiveNode = Nothing
    }


validFL :: FerryLink -> Bool
validFL FL{..} =
       flNegativeNode /= Nothing
    && flPositiveNode /= Nothing


root :: Int -> Transition
root index (StartElement "osgb:networkMember" _) =
    await (networkMember index)
root index _ =
    await (root index)


networkMember :: Int -> Transition
networkMember index (EndElement "osgb:networkMember") =
    await (root index)
networkMember index (StartElement "osgb:FerryLink" attrs) =
    await (ferryLink (newFL index (getTOID attrs)))
networkMember index _ =
    await (networkMember index)


ferryLink :: FerryLink -> Transition
ferryLink fl (EndElement "osgb:FerryLink")
  | validFL fl =
        yield fl (networkMember (flIndex fl + 1))
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
