module Attributes where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as T


type Attributes =
    [(ByteString, ByteString)]


getTOID :: Attributes -> Text
getTOID attrs =
    case lookup "fid" attrs of
      Just toid ->
        T.decodeUtf8 toid
      Nothing ->
        error ("getTOID: missing fid: " ++ show attrs)


getHRef :: Attributes -> Text
getHRef attrs =
    case lookup "xlink:href" attrs of
      Just ref
        | B.length ref == 21 ->
            T.decodeUtf8 (B.tail ref)
      _ ->
        error ("getHRef: missing xlink:href: " ++ show attrs)


getDirectedNode :: Attributes -> Either Text Text
getDirectedNode attrs =
    let
      toid = getHRef attrs
    in
      case lookup "orientation" attrs of
        Just "-" ->
          Left toid
        Just "+" ->
          Right toid
        _ ->
          error ("getDirectedNode: missing orientation: " ++ show attrs)
