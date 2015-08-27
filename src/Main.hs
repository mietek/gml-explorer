{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashSet as S
import Data.List (intersperse, mapAccumL, sort)
import Data.Maybe (catMaybes)
import Text.XML.Expat.SAX (ParseOptions(..), SAXEvent(..))
import qualified Text.XML.Expat.SAX as X

import MealyMachine
import Options
import Points
import Polylines


main :: IO ()
main = do
    opts <- getOptions
    xml <- L.readFile (optInput opts)
    let results = process (optCommand opts) xml
    mapM_ B.putStr results


process :: Command -> L.ByteString -> [ByteString]
process Tags      = intersperse "\n" . unique . stream tags
process AttrKeys  = intersperse "\n" . unique . stream attrKeys
process Polylines = statefulStream (newMealyMachine polylines)
process Points    = statefulStream (newMealyMachine points)


unique :: [ByteString] -> [ByteString]
unique = sort . S.toList . S.fromList


stream :: (Event -> [ByteString]) -> L.ByteString -> [ByteString]
stream fun = concatMap fun . parse

statefulStream :: MealyMachine -> L.ByteString -> [ByteString]
statefulStream mm = catMaybes . snd . mapAccumL runMealyMachine mm . parse


parse :: L.ByteString -> [Event]
parse = X.parse (ParseOptions Nothing Nothing)


tags :: Event -> [ByteString]
tags (StartElement tag _) = [tag]
tags _ = []

attrKeys :: Event -> [ByteString]
attrKeys (StartElement _ attrs) = map fst attrs
attrKeys _ = []
