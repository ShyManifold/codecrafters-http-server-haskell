module DataCompresion (encode) where

import qualified Data.ByteString.Lazy as BL

import Codec.Compression.GZip (compress)
import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString)
import qualified Data.ByteString as BL

encode :: ByteString->ByteString
encode = BL.toStrict . compress . BL.fromStrict