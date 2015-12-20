{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module RoadNodes (root) where

import Text.XML.Expat.SAX (SAXEvent(..))

import ITN
import MealyMachine


root (StartElement "osgb:networkMember" _) = await networkMember
root _ = await root

networkMember (EndElement "osgb:networkMember")    = await root
networkMember (StartElement "osgb:RoadNode" attrs) = yieldTOID attrs roadNode
networkMember _ = await networkMember

roadNode (EndElement "osgb:RoadNode")  = yield "\n" networkMember
roadNode (StartElement "osgb:point" _) = await osgbPoint
roadNode _ = await roadNode

osgbPoint (EndElement "osgb:point")    = await roadNode
osgbPoint (StartElement "gml:Point" _) = await gmlPoint
osgbPoint _ = await osgbPoint

gmlPoint (EndElement "gml:Point")           = await osgbPoint
gmlPoint (StartElement "gml:coordinates" _) = await coordinates
gmlPoint _ = await gmlPoint

coordinates (EndElement "gml:coordinates") = await gmlPoint
coordinates (CharacterData part)           = yield part coordinates
coordinates _ = await coordinates
