{-# LANGUAGE OverloadedStrings #-}

module RoadNodes (roadNodes) where

import Text.XML.Expat.SAX (SAXEvent(..))

import MealyMachine
import TOID


roadNodes :: Transition
roadNodes = startRoadNode

startRoadNode :: Transition
startRoadNode (StartElement "osgb:RoadNode" attrs) = yieldTOID attrs startOSGBPoint
startRoadNode _                                    = await startRoadNode

startOSGBPoint :: Transition
startOSGBPoint (StartElement "osgb:point" _) = await startGMLPoint
startOSGBPoint _                             = await startOSGBPoint

startGMLPoint :: Transition
startGMLPoint (StartElement "gml:Point" _) = await startCoordinates
startGMLPoint _                            = await startGMLPoint

startCoordinates :: Transition
startCoordinates (StartElement "gml:coordinates" _) = await characterData
startCoordinates _                                  = await startCoordinates

characterData :: Transition
characterData (CharacterData part)           = yield part characterData
characterData (EndElement "gml:coordinates") = yield "\n" endGMLPoint
characterData _                              = await characterData

endGMLPoint :: Transition
endGMLPoint (EndElement "gml:Point") = await endOSGBPoint
endGMLPoint _                        = await endGMLPoint

endOSGBPoint :: Transition
endOSGBPoint (EndElement "osgb:point") = await endRoadNode
endOSGBPoint _                         = await endOSGBPoint

endRoadNode :: Transition
endRoadNode (EndElement "osgb:RoadNode") = await startRoadNode
endRoadNode _                            = await endRoadNode
