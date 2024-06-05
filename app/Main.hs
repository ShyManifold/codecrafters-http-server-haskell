{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Control.Monad (forever)

import Control.Concurrent (forkIO)

import qualified Data.ByteString.Char8 as BC
import Network.Socket
import Network.Socket.ByteString (send, recv)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import HttpRequest
import HttpResponses
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering


    let host = "127.0.0.1"
        port = "4221"

    BC.putStrLn $ "Listening on " <> BC.pack host <> ":" <> BC.pack port

    -- Get address information for the given host and port

    addrInfo <- getAddrInfo Nothing (Just host) (Just port)

    serverSocket <- socket (addrFamily $ head addrInfo) Stream defaultProtocol

    setSocketOption serverSocket ReuseAddr 1

    withFdSocket serverSocket setCloseOnExecIfNeeded

    bind serverSocket $ addrAddress $ head addrInfo

    listen serverSocket 5

    -- Accept connections and handle them forever

    forever $ do

        (clientSocket, clientAddr) <- accept serverSocket

        BC.putStrLn $ "Accepted connection from " <> BC.pack (show clientAddr) <> "."

        forkIO $ handleConnection clientSocket


handleConnection :: Socket -> IO ()
handleConnection soc = do

    requestData <- recv soc 4096

    BC.putStrLn $ "Received: " <> requestData

    let parsedRequest = parseHttpRequest requestData

    print $ "Parsed request:\n" <> show parsedRequest

    response <- respondRequest parsedRequest

    BC.putStrLn $ "Responded with: " <> response <> "\n--End of response--"

    -- Handle the clientSocket as needed...
    _ <- send soc response

    close soc