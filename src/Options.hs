module Options where

import Options.Applicative ((<>), Parser)
import qualified Options.Applicative as P


data Options = Opts
    { inFileOpt  :: FilePath
    , outDirOpt  :: FilePath
    , maxSizeOpt :: Int
    , cmdOpt     :: Command
    }
  deriving (Eq, Show)


data Command =
    Roads
  | RoadLinks
  | RoadNodes
  | FerryLinks
  | FerryNodes
  deriving (Eq, Ord, Show)


getOptions :: IO Options
getOptions =
    P.execParser $
      P.info (P.helper <*> parseOptions) $
           P.header "gml-explorer"
        <> P.progDesc "Explore an OS GML file"
        <> P.fullDesc


parseOptions :: Parser Options
parseOptions = Opts <$>
        parseInputFile
    <*> parseOutputDir
    <*> parseMaxFileSize
    <*> parseCommand


parseInputFile :: Parser FilePath
parseInputFile =
    P.argument P.str $
         P.metavar "INPUT_FILE"
      <> P.help "File containing OS GML input"


parseOutputDir :: Parser FilePath
parseOutputDir =
    P.strOption $
         P.metavar "OUTPUT_DIR"
      <> P.short 'o'
      <> P.value "dist/out"
      <> P.help "Output directory"


parseMaxFileSize :: Parser Int
parseMaxFileSize =
    P.option P.auto $
         P.metavar "MAX_FILE_SIZE"
      <> P.short 's'
      <> P.value (30 * 1024 * 1024)
      <> P.help "Maximum size of file to output (bytes)"


parseCommand :: Parser Command
parseCommand =
    P.subparser $
         command "roads" "Output OS Road geometry" Roads
      <> command "roadlinks" "Output OS RoadLink geometry" RoadLinks
      <> command "roadnodes" "Output OS RoadNode geometry" RoadNodes
      <> command "ferrylinks" "Output OS FerryLink geometry" FerryLinks
      <> command "ferrynodes" "Output OS FerryNode geometry" FerryNodes


command :: String -> String -> Command -> P.Mod P.CommandFields Command
command name desc cmd =
    P.command name
      (P.info
        (P.helper <*> P.pure cmd)
        (P.progDesc desc))
