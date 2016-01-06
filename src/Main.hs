module Main where

import Data.ByteString (ByteString)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (Handle, IOMode(..), withFile)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T

import MealyMachine
import Options
import qualified Roads
import qualified RoadLinks
import qualified RoadNodes


main :: IO ()
main = do
    Opts{..} <- getOptions
    xml <- L.readFile inFileOpt
    let
      root = dispatch cmdOpt
      items = runMM (newMM root) xml
    createDirectoryIfMissing True outDirOpt
    writeFiles outDirOpt cmdOpt items maxSizeOpt


dispatch :: Command -> Transition
dispatch Roads =
    Roads.root
dispatch RoadLinks =
    RoadLinks.root
dispatch RoadNodes =
    RoadNodes.root


writeFiles :: FilePath -> Command -> [ByteString] -> Int -> IO ()
writeFiles outDir cmd allItems maxSize =
    loop allItems 1
  where
    loop [] _ =
        return ()
    loop items fileIx = do
        let
          outFile = outputFile outDir cmd fileIx
        restItems <- withFile outFile WriteMode $ \h ->
          writeItems items maxSize h
        loop restItems (fileIx + 1)


outputFile :: FilePath -> Command -> Int -> FilePath
outputFile outDir cmd fileIx =
    outDir </> toLower (show cmd) ++ show fileIx ++ ".json"
  where
    toLower =
        T.unpack . T.toLower . T.pack


writeItems :: [ByteString] -> Int -> Handle -> IO [ByteString]
writeItems [] _ h = do
    B.hPutStrLn h "[]"
    return []
writeItems (firstItem : otherItems) maxSize h = do
    B.hPutStr h "["
    B.hPutStrLn h firstItem
    restItems <- loop otherItems maxSize
    B.hPutStrLn h "]"
    return restItems
  where
    loop [] _ =
        return []
    loop items@(item : moreItems) size = do
        let
          nextSize = size - B.length item
        if nextSize >= 0
          then do
            B.hPutStr h ","
            B.hPutStrLn h item
            loop moreItems nextSize
          else
            return items
