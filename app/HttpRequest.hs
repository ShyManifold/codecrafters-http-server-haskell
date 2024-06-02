{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE GADTs #-}

module HttpRequest (respondRequest, parseHttpRequest) where

import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString)

data HttpRequest = HttpRequest { status :: HttpStatus }

data HttpStatus = HttpStatus { method :: ByteString
                             , path :: ByteString
                             , version :: ByteString
                             }
--data HttpBody = HttpBody {}

parseHttpRequest :: ByteString -> HttpRequest
parseHttpRequest raw =
  let (m:p:v:_) = BC.words . head $ BC.lines raw
  in HttpRequest (HttpStatus m p v)

respondRequest :: HttpRequest -> ByteString
respondRequest req =
    case path (status req) of
        "/" -> "HTTP/1.1 200 OK\r\n\r\n"
        p | "/echo/" `BC.isPrefixOf` p -> echoRespond (status req)
        _ -> "HTTP/1.1 404 Not Found\r\n\r\n"

echoRespond :: HttpStatus -> ByteString
echoRespond httpStatus = 
     "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: "<> echoLength <> "\r\n\r\n" <> echo
     where
        echo = BC.drop 6 (path httpStatus)
        echoLength = BC.pack . show $ BC.length (path httpStatus) - 6 --Subtract /echo/