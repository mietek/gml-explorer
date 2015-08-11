module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.List
import Data.Maybe
import qualified Data.HashSet as S
import System.Environment
import System.Exit
import System.IO
import Text.XML.Expat.SAX


main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> process path
        _ -> do
            hPutStrLn stderr "Usage: gml-explorer <XML file>"
            exitFailure

process :: FilePath -> IO ()
process path = do
    xml <- L.readFile path
    let tags = parseTags xml
    print tags

parseTags :: L.ByteString -> [ByteString]
parseTags =
      sort
    . S.toList
    . S.fromList
    . catMaybes
    . map startElementTag
    . parse opts
  where
    opts = ParseOptions Nothing Nothing

startElementTag :: SAXEvent ByteString ByteString -> Maybe ByteString
startElementTag (StartElement tag _attrs) = Just tag
startElementTag _ = Nothing
