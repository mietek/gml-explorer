{-# LANGUAGE OverloadedStrings #-}

module Points where

import Text.XML.Expat.SAX (SAXEvent(..))

import MealyMachine


points_startRoadNode :: Transition
points_startRoadNode (StartElement "osgb:RoadNode" _) = await points_startOSGBPoint
points_startRoadNode _                                = await points_startRoadNode

points_startOSGBPoint :: Transition
points_startOSGBPoint (StartElement "osgb:point" _) = await points_startGMLPoint
points_startOSGBPoint _                             = await points_startOSGBPoint

points_startGMLPoint :: Transition
points_startGMLPoint (StartElement "gml:Point" _) = await points_startCoordinates
points_startGMLPoint _                            = await points_startGMLPoint

points_startCoordinates :: Transition
points_startCoordinates (StartElement "gml:coordinates" _) = await points_characterData
points_startCoordinates _                                  = await points_startCoordinates

points_characterData :: Transition
points_characterData (CharacterData part)           = yield part points_characterData
points_characterData (EndElement "gml:coordinates") = yield "\n" points_endGMLPoint
points_characterData _                              = await points_characterData

points_endGMLPoint :: Transition
points_endGMLPoint (EndElement "gml:Point") = await points_endOSGBPoint
points_endGMLPoint _                        = await points_endGMLPoint

points_endOSGBPoint :: Transition
points_endOSGBPoint (EndElement "osgb:point") = await points_endRoadNode
points_endOSGBPoint _                         = await points_endOSGBPoint

points_endRoadNode :: Transition
points_endRoadNode (EndElement "osgb:RoadNode") = await points_startRoadNode
points_endRoadNode _                            = await points_endRoadNode
