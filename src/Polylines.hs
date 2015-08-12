{-# LANGUAGE OverloadedStrings #-}

module Polylines where

import Text.XML.Expat.SAX (SAXEvent(..))

import MealyMachine


polylines_startRoadLink :: Transition
polylines_startRoadLink (StartElement "osgb:RoadLink" _) = await polylines_startPolyline
polylines_startRoadLink _                                = await polylines_startRoadLink

polylines_startPolyline :: Transition
polylines_startPolyline (StartElement "osgb:polyline" _) = await polylines_startLineString
polylines_startPolyline _                                = await polylines_startPolyline

polylines_startLineString :: Transition
polylines_startLineString (StartElement "gml:LineString" _) = await polylines_startCoordinates
polylines_startLineString _                                 = await polylines_startLineString

polylines_startCoordinates :: Transition
polylines_startCoordinates (StartElement "gml:coordinates" _) = await polylines_characterData
polylines_startCoordinates _                                  = await polylines_startCoordinates

polylines_characterData :: Transition
polylines_characterData (CharacterData part)           = yield part polylines_characterData
polylines_characterData (EndElement "gml:coordinates") = yield "\n" polylines_endLineString
polylines_characterData _                              = await polylines_characterData

polylines_endLineString :: Transition
polylines_endLineString (EndElement "gml:LineString") = await polylines_endPolyline
polylines_endLineString _                             = await polylines_endLineString

polylines_endPolyline :: Transition
polylines_endPolyline (EndElement "osgb:polyline") = await polylines_endRoadLink
polylines_endPolyline _                            = await polylines_endPolyline

polylines_endRoadLink :: Transition
polylines_endRoadLink (EndElement "osgb:RoadLink") = await polylines_startRoadLink
polylines_endRoadLink _                            = await polylines_endRoadLink
