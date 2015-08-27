module Options where

import Options.Applicative ((<$>), (<*>), (<>), Parser)
import qualified Options.Applicative as O


data Options = Options
  { optInput   :: FilePath
  , optCommand :: Command
  }
  deriving (Eq, Show)

data Command =
    Tags
  | AttrKeys
  | RoadLinks
  | RoadNodes
  deriving (Eq, Ord, Show)


getOptions :: IO Options
getOptions =
    O.execParser $
      O.info (O.helper <*> parseOptions)
         ( O.header "gml-explorer"
        <> O.progDesc "Explore an OS GML file"
        <> O.fullDesc
         )

parseOptions :: Parser Options
parseOptions =
    Options <$>
          parseInput
      <*> parseCommand

parseInput :: Parser FilePath
parseInput =
    O.argument O.str
       ( O.metavar "INPUT"
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
