{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BC
import Network.Socket
import Network.Socket.ByteString (send, recv)
import System.IO (BufferMode (..), hSetBuffering, stdout)

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

        requestData <- recv clientSocket 1024

        let response = case BC.words requestData of
                (_:"/":_) -> "HTTP/1.1 200 OK\r\n\r\n" 
                _ -> "HTTP/1.1 404 Not Found\r\n\r\n"

        -- Handle the clientSocket as needed...
        _ <- send clientSocket response

        close clientSocket