{-# LANGUAGE OverloadedStrings #-}

module Polylines (polylines) where

import Text.XML.Expat.SAX (SAXEvent(..))

import MealyMachine


polylines :: Transition
polylines = startRoadLink

startRoadLink :: Transition
startRoadLink (StartElement "osgb:RoadLink" _) = await startPolyline
startRoadLink _                                = await startRoadLink

startPolyline :: Transition
startPolyline (StartElement "osgb:polyline" _) = await startLineString
startPolyline _                                = await startPolyline

startLineString :: Transition
startLineString (StartElement "gml:LineString" _) = await startCoordinates
startLineString _                                 = await startLineString

startCoordinates :: Transition
startCoordinates (StartElement "gml:coordinates" _) = await characterData
startCoordinates _                                  = await startCoordinates

characterData :: Transition
characterData (CharacterData part)           = yield part characterData
characterData (EndElement "gml:coordinates") = yield "\n" endLineString
characterData _                              = await characterData

endLineString :: Transition
endLineString (EndElement "gml:LineString") = await endPolyline
endLineString _                             = await endLineString

endPolyline :: Transition
endPolyline (EndElement "osgb:polyline") = await endRoadLink
endPolyline _                            = await endPolyline

endRoadLink :: Transition
endRoadLink (EndElement "osgb:RoadLink") = await startRoadLink
endRoadLink _                            = await endRoadLink
