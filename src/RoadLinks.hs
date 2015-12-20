{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module RoadLinks (root) where

import Text.XML.Expat.SAX (SAXEvent(..))

import ITN
import MealyMachine


root (StartElement "osgb:networkMember" _) = await networkMember
root _ = await root

networkMember (EndElement "osgb:networkMember")    = await root
networkMember (StartElement "osgb:RoadLink" attrs) = yieldTOID attrs roadLink
networkMember _ = await networkMember

roadLink (EndElement "osgb:RoadLink")             = yield "\n" networkMember
roadLink (StartElement "osgb:length" _)           = await osgbLength
roadLink (StartElement "osgb:polyline" _)         = await polyline
roadLink (StartElement "osgb:directedNode" attrs) = yieldDirectedNode attrs roadLink
roadLink _ = await roadLink

osgbLength (EndElement "osgb:length") = yield " " lineString
osgbLength (CharacterData part)       = yield part osgbLength
osgbLength _ = await osgbLength

polyline (EndElement "osgb:polyline")      = await roadLink
polyline (StartElement "gml:LineString" _) = await lineString
polyline _ = await polyline

lineString (EndElement "gml:LineString")      = await polyline
lineString (StartElement "gml:coordinates" _) = await coordinates
lineString _ = await lineString

coordinates (EndElement "gml:coordinates") = yield " " lineString
coordinates (CharacterData part)           = yield part coordinates
coordinates _ = await coordinates
