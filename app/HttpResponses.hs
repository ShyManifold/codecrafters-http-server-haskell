{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE GADTs #-}

module HttpResponses (respondRequest) where

import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString)
import HttpRequest

import System.Environment (getArgs)
import System.IO.Error (tryIOError)

respondRequest :: HttpRequest -> IO ByteString
respondRequest req =
    case path (status req) of
        "/" -> pure "HTTP/1.1 200 OK\r\n\r\n"
        p | "/echo/" `BC.isPrefixOf` p ->pure $ echoResponse (status req)
        p | "/user-agent" `BC.isPrefixOf` p -> pure $ userAgentResponse req
        p | "/file" `BC.isPrefixOf` p -> sendFileResponse
        _ -> pure "HTTP/1.1 404 Not Found\r\n\r\n"

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
        userAgentLength = BC.pack . show $ BC.length userAgent

sendFileResponse::IO ByteString
sendFileResponse = do
    args <- getArgs
    case args of
        ["--directory", filepath] -> do
            (exists, maybeContents) <- fileContentsIfExists filepath
            if exists
                then case maybeContents of
                    Just contents -> pure $ makeSendFileResponse (length contents) contents
                    Nothing -> pure $ makeSendFileResponse 0 "" --File exists but it is empty
                else pure "HTTP/1.1 404 Not Found\r\n\r\n" --File doesn't exist
        _ -> pure "HTTP/1.1 400 Bad Request\r\n\r\n" -- Called with the wrong arguments
    where
        makeSendFileResponse::Int -> String->ByteString
        makeSendFileResponse len contents = 
            BC.pack $ "HTTP/1.1 200 OK\r\nContent-Type: application/octet-stream\r\nContent-Length: " <> show len <> "\r\n\r\n" <> contents
    
fileContentsIfExists :: FilePath -> IO (Bool, Maybe String)
fileContentsIfExists filepath = do
    result <- tryIOError (readFile filepath)
    case result of
        Left _  -> return (False, Nothing)
        Right contents -> return (True, Just contents)



--where
--        handleFile::String->IO ByteString
--        handleFile fp = do
--            fileContents <- readFile fp
--            fileExist <- tryIOError $ getFileStatus filePath
--            file_size <- fromIntegral $ fileSize status
--            "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: " <> show file_size <> "\r\n\r\n" <> fileContents