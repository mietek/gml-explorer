{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.List (sort)
import qualified Data.HashSet as S
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
       )


process :: Command -> L.ByteString -> [ByteString]
process Tags      = unique . stream elementTag
process AttrKeys  = unique . stream elementAttrKeys


unique :: [ByteString] -> [ByteString]
unique = sort . S.toList . S.fromList


stream :: (SAXEvent ByteString ByteString -> [ByteString]) -> L.ByteString -> [ByteString]
stream fun = concatMap fun . X.parse (ParseOptions Nothing Nothing)

elementTag :: SAXEvent ByteString ByteString -> [ByteString]
elementTag (StartElement tag _) = [tag]
elementTag _ = []

elementAttrKeys :: SAXEvent ByteString ByteString -> [ByteString]
elementAttrKeys (StartElement _ attrs) = map fst attrs
elementAttrKeys _ = []
