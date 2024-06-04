{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE GADTs #-}

module HttpResponses (respondRequest) where

import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString, breakSubstring, stripPrefix, takeWhileEnd)
import HttpRequest

respondRequest :: HttpRequest -> ByteString
respondRequest req =
    case path (status req) of
        "/" -> "HTTP/1.1 200 OK\r\n\r\n"
        p | "/echo/" `BC.isPrefixOf` p -> echoResponse (status req)
        p | "/user-agent" `BC.isPrefixOf` p -> userAgentResponse req
        _ -> "HTTP/1.1 404 Not Found\r\n\r\n"

echoResponse :: HttpStatus -> ByteString
echoResponse httpStatus = 
     "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: "<> echoLength <> "\r\n\r\n" <> echo
     where
        echo = BC.drop 6 (path httpStatus)
        echoLength = BC.pack . show $ BC.length (path httpStatus) - 6 --Subtract /echo/

userAgentResponse::HttpRequest -> ByteString
userAgentResponse (HttpRequest _ h _) =
    "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: " <> userAgentLength <> "\r\n\r\n" <> userAgent
    where
        userAgent = getHeaderValue UserAgent h
        userAgentLength = BC.pack . show $ BC.length (userAgent)