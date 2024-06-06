{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HttpResponses (respondRequest) where

import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString, isInfixOf)
import HttpRequest

import Control.Exception (try, IOException)

import ConfigurationParser

import DataCompresion

respondRequest :: HttpRequest -> ServerOptions -> IO ByteString
respondRequest req config =
    case method $ status req of
        "GET" -> handleGetRequest req config
        "POST" -> handlePostRequest req config
        _ -> pure "HTTP/1.1 404 Bad Request\r\n\r\n"

------------------------------------------------------------------------
--------------------Functions to parse a GET request--------------------
------------------------------------------------------------------------

handleGetRequest::HttpRequest -> ServerOptions -> IO ByteString
handleGetRequest req config =
    case path (status req) of
        "/" -> pure "HTTP/1.1 200 OK\r\n\r\n"
        p | "/echo/" `BC.isPrefixOf` p ->pure $ echoResponse req
        p | "/user-agent" `BC.isPrefixOf` p -> pure $ userAgentResponse req
        p | "/file" `BC.isPrefixOf` p -> sendFileResponse (status req) (serverDirectory config)
        _ -> pure "HTTP/1.1 404 Not Found\r\n\r\n"

echoResponse :: HttpRequest -> ByteString
echoResponse (HttpRequest httpStatus httpHeaders _) =
     if hasGzipEncoding
        then "HTTP/1.1 200 OK\r\nContent-Encoding: gzip\r\nContent-Type: text/plain\r\nContent-Length: "<> encodedLength <> "\r\n\r\n" <> encodedEcho
        else "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: "<> echoLength <> "\r\n\r\n" <> echo
     where
        (hasAcceptEncodingHeader, maybeHeaderValue) = getValueIfHasHeader AcceptEncoding httpHeaders
        hasGzipEncoding =
            hasAcceptEncodingHeader && (case maybeHeaderValue of
                    Just hValue -> "gzip" `isInfixOf` hValue
                    Nothing -> False)
        echo = BC.drop 6 (path httpStatus)
        echoLength = BC.pack . show $ BC.length (path httpStatus) - 6 --Subtract /echo/
        encodedEcho = encode echo
        encodedLength = BC.pack . show $ BC.length encodedEcho

userAgentResponse::HttpRequest -> ByteString
userAgentResponse (HttpRequest _ h _) =
    "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: " <> userAgentLength <> "\r\n\r\n" <> userAgent
    where
        userAgent = getHeaderValue UserAgent h
        userAgentLength = BC.pack . show $ BC.length userAgent

sendFileResponse::HttpStatus -> FilePath -> IO ByteString
sendFileResponse httpStatus dir = do
    (exists, maybeContents) <- fileContentsIfExists filepath
    if exists
        then case maybeContents of
            Just contents -> pure $ makeSendFileResponse (length contents) contents
            Nothing -> pure $ makeSendFileResponse 0 "" --File exists but it is empty
        else pure "HTTP/1.1 404 Not Found\r\n\r\n" --File doesn't exist
    where
        filepath = dir <> ( BC.unpack . BC.drop 6 $ path httpStatus)
        makeSendFileResponse::Int -> String->ByteString
        makeSendFileResponse len contents =
            BC.pack $ "HTTP/1.1 200 OK\r\nContent-Type: application/octet-stream\r\nContent-Length: " <> show len <> "\r\n\r\n" <> contents

fileContentsIfExists :: FilePath -> IO (Bool, Maybe String)
fileContentsIfExists filepath = do
    result <- try $ readFile filepath
    case result of
        Left (ex :: IOException) -> do
            putStrLn $ "Error reading file: " ++ show ex
            return (False, Nothing)
        Right contents ->
            return (True, Just contents)

------------------------------------------------------------------------
--------------------Functions to parse a POST request-------------------
------------------------------------------------------------------------

handlePostRequest::HttpRequest -> ServerOptions -> IO ByteString
handlePostRequest req config =
    case path (status req) of
        p | "/file" `BC.isPrefixOf` p -> uploadFileResponse req (serverDirectory config)
        _ -> pure "HTTP/1.1 404 Not Found\r\n\r\n"

uploadFileResponse :: HttpRequest -> FilePath -> IO ByteString
uploadFileResponse (HttpRequest reqStatusLine _ (HttpBody reqBody)) dir = do
    -- Could add a check for whether the file exists or not and handle each case
    let filepath = dir <> (BC.unpack . BC.drop 6 $ path reqStatusLine)

    result <- try $ writeFile filepath (BC.unpack reqBody)

    print $ "Parsed filepath: " <> filepath

    case result of
        Left (ex :: IOException) -> do
            print $ "Error: " <> show ex
            pure "HTTP/1.1 500 Internal Server Error\r\n\r\n" -- Or any error response you prefer
        Right _ ->
            pure "HTTP/1.1 201 Created\r\n\r\n"

