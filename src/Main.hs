{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.HashSet as S
import Data.List (mapAccumL, sort)
import Data.Maybe (catMaybes)
import Options.Applicative ((<$>), (<*>), (<>), Parser)
import qualified Options.Applicative as O
import Text.XML.Expat.SAX (ParseOptions(..), SAXEvent(..))
import qualified Text.XML.Expat.SAX as X


data Options = Options
  { optInput   :: FilePath
  , optCommand :: Command
  }
  deriving (Eq, Show)

data Command =
    Tags
  | AttrKeys
  | Polylines
  | Points
  deriving (Eq, Ord, Show)


main :: IO ()
main = do
    opts <- O.execParser $
      O.info (O.helper <*> parseOptions)
         ( O.header "gml-explorer"
        <> O.progDesc "Explore an OS GML file"
        <> O.fullDesc
         )
    xml <- L.readFile (optInput opts)
    let results = process (optCommand opts) xml
    mapM_ C.putStrLn results


parseOptions :: Parser Options
parseOptions =
    Options <$>
          parseInput
      <*> parseCommand

parseInput :: Parser FilePath
parseInput =
    O.argument O.str
       ( O.metavar "FILEPATH"
      <> O.help "File containing OS GML input"
       )

parseCommand :: Parser Command
parseCommand =
    O.subparser
       ( O.command "tags"
           (O.info (O.helper <*> O.pure Tags)
           (O.progDesc "Output unique tags"))
      <> O.command "attrkeys"
           (O.info (O.helper <*> O.pure AttrKeys)
           (O.progDesc "Output unique attribute keys"))
      <> O.command "polylines"
           (O.info (O.helper <*> O.pure Polylines)
           (O.progDesc "Output all OS RoadLink polylines"))
      <> O.command "points"
           (O.info (O.helper <*> O.pure Points)
           (O.progDesc "Output all OS RoadNode points"))
       )


process :: Command -> L.ByteString -> [ByteString]
process Tags      = unique . stream elementTag
process AttrKeys  = unique . stream elementAttrKeys
process Polylines = statefulStream (startMealyMachine polylines_startRoadLink)
process Points    = statefulStream (startMealyMachine points_startRoadNode)


unique :: [ByteString] -> [ByteString]
unique = sort . S.toList . S.fromList


type Event = SAXEvent ByteString ByteString

parse :: L.ByteString -> [Event]
parse = X.parse (ParseOptions Nothing Nothing)


stream :: (Event -> [ByteString]) -> L.ByteString -> [ByteString]
stream fun = concatMap fun . parse

elementTag :: Event -> [ByteString]
elementTag (StartElement tag _) = [tag]
elementTag _ = []

elementAttrKeys :: Event -> [ByteString]
elementAttrKeys (StartElement _ attrs) = map fst attrs
elementAttrKeys _ = []


{-

G.H. Mealy, “A method for synthesizing sequential circuits”, Bell System Technical Journal, 1955, pp. 1045–1079

-}

newtype MealyMachine = MM
  { runMealyMachine :: Transition
  }

type Transition = Event -> (MealyMachine, Maybe ByteString)

startMealyMachine :: Transition -> MealyMachine
startMealyMachine next = MM next

await :: Transition -> (MealyMachine, Maybe ByteString)
await next = (MM next, Nothing)

andAwait :: ByteString -> Transition -> (MealyMachine, Maybe ByteString)
andAwait result next = (MM next, Just result)

statefulStream :: MealyMachine -> L.ByteString -> [ByteString]
statefulStream m = catMaybes . snd . mapAccumL runMealyMachine m . parse


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
polylines_characterData (CharacterData yield) = yield `andAwait` polylines_endCoordinates
polylines_characterData _                     = await polylines_characterData

polylines_endCoordinates :: Transition
polylines_endCoordinates (EndElement "gml:coordinates") = await polylines_endLineString
polylines_endCoordinates _                              = await polylines_endCoordinates

polylines_endLineString :: Transition
polylines_endLineString (EndElement "gml:LineString") = await polylines_endPolyline
polylines_endLineString _                             = await polylines_endLineString

polylines_endPolyline :: Transition
polylines_endPolyline (EndElement "osgb:polyline") = await polylines_endRoadLink
polylines_endPolyline _                            = await polylines_endPolyline

polylines_endRoadLink :: Transition
polylines_endRoadLink (EndElement "osgb:RoadLink") = await polylines_startRoadLink
polylines_endRoadLink _                            = await polylines_endRoadLink


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
points_characterData (CharacterData yield) = yield `andAwait` points_endCoordinates
points_characterData _                     = await points_characterData

points_endCoordinates :: Transition
points_endCoordinates (EndElement "gml:coordinates") = await points_endGMLPoint
points_endCoordinates _                              = await points_endCoordinates

points_endGMLPoint :: Transition
points_endGMLPoint (EndElement "gml:Point") = await points_endOSGBPoint
points_endGMLPoint _                        = await points_endGMLPoint

points_endOSGBPoint :: Transition
points_endOSGBPoint (EndElement "osgb:point") = await points_endRoadNode
points_endOSGBPoint _                         = await points_endOSGBPoint

points_endRoadNode :: Transition
points_endRoadNode (EndElement "osgb:RoadNode") = await points_startRoadNode
points_endRoadNode _                            = await points_endRoadNode
