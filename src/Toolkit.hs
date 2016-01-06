module Toolkit
  ( Builder
  , none
  , (<>)
  , build
  , encodeJSON
  , decodeDouble
  , decodePoint
  , decodePolyline
  ) where

import Data.Aeson (ToJSON)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.Text (Text)
import qualified Data.Aeson as J
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Monoid as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T


none :: Builder
none =
    mempty


(<>) :: Builder -> ByteString -> Builder
parts <> part =
    parts M.<> B.byteString part


build :: Builder -> Text
build =
    T.decodeUtf8 . L.toStrict . B.toLazyByteString


encodeJSON :: (ToJSON a) => a -> ByteString
encodeJSON =
    L.toStrict . J.encode


decodeDouble :: Text -> Double
decodeDouble t =
    case T.double t of
      Right (d, "") ->
        d
      Right _ ->
        error ("decodeDouble: invalid double: " ++ show t)
      Left msg ->
        error ("decodeDouble: " ++ msg ++ ": " ++ show t)


decodePoint :: Text -> [Double]
decodePoint t =
    case T.splitOn "," t of
      [tx, ty] ->
        [decodeDouble tx, decodeDouble ty]
      _ ->
        error ("decodePoint: invalid point: " ++ show t)


decodePolyline :: Text -> [Double]
decodePolyline t
  | l >= 2 =
        concatMap decodePoint ts
  | otherwise =
        error ("decodePolyline: invalid polyline: " ++ show t)
  where
    ts = T.splitOn " " t
    l = length ts
