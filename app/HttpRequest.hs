{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module HttpRequest ( HttpRequest(..),
                     HttpStatus(..),
                     HttpBody(..),
                     HeaderType(..),
                     parseHttpRequest,
                     getHeaderValue,
                     getValueIfHasHeader) where

import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString, breakSubstring, isPrefixOf)
import Data.Char (toLower)

import Control.Exception (Exception, throw )

data HttpRequest = HttpRequest { status :: HttpStatus, headers :: [HttpHeader], body::HttpBody}

data HttpStatus = HttpStatus { method :: ByteString
                             , path :: ByteString
                             , version :: ByteString
                             }

newtype HttpBody = HttpBody ByteString

data HttpHeader = HttpHeader { headerType::HeaderType
                             , headerValue::ByteString}

data HeaderType
    = UserAgent
    | Host
    | Connection
    | AcceptEncoding
    | Accept
    | Other deriving (Eq)
-- Http 1.1 header types
--General Headers:
--
--    Cache-Control
--    Connection
--    Date
--    Pragma
--    Trailer
--    Transfer-Encoding
--    Upgrade
--    Via
--    Warning
--
--Request Headers:
--
--    Accept
--    Accept-Charset
--    Accept-Encoding
--    Accept-Language
--    Authorization
--    Expect
--    From
--    Host
--    If-Match
--    If-Modified-Since
--    If-None-Match
--    If-Range
--    If-Unmodified-Since
--    Max-Forwards
--    Proxy-Authorization
--    Range
--    Referer
--    TE
--    User-Agent
--
--Response Headers:
--
--    Accept-Ranges
--    Age
--    ETag
--    Location
--    Proxy-Authenticate
--    Retry-After
--    Server
--    Vary
--    WWW-Authenticate
--
--Entity Headers:
--
--    Allow
--    Content-Encoding
--    Content-Language
--    Content-Length
--    Content-Location
--    Content-MD5
--    Content-Range
--    Content-Type
--    Expires
--    Last-Modified

parseHttpRequest :: ByteString -> HttpRequest
parseHttpRequest raw =
  HttpRequest (HttpStatus m p v) h (HttpBody $ BC.drop 4 bodyString)
  where
    (statusLine_, rest) = breakSubstring "\r\n" raw
    (m:p:v:_) = BC.words statusLine_
    (headerString, bodyString) =  breakSubstring "\r\n\r\n" rest
    h = parseHeaders headerString

parseHeaders :: ByteString -> [HttpHeader]
parseHeaders req = p req []
    where
        p :: ByteString -> [HttpHeader] -> [HttpHeader]
        p request headerArray =
            let
                (headerString, _) = breakSubstring "\r\n\r\n" request
                headers_ = BC.lines headerString
                parsedHeaders = map parseHeader headers_
            in
                parsedHeaders ++ headerArray

parseHeader :: ByteString -> HttpHeader
parseHeader header =
    let
        (hType, rest) = breakSubstring ":" header
        hValue = BC.dropWhile (== ' ') (BC.drop 1 rest) -- Drop the space after ':'
    in
        case hType of
            t | "user-agent" `isPrefixOf` lowerByteString t -> HttpHeader UserAgent hValue
            t | "host" `isPrefixOf` lowerByteString t -> HttpHeader Host hValue
            t | "connection" `isPrefixOf` lowerByteString t -> HttpHeader Connection hValue
            t | "accept-encoding" `isPrefixOf` lowerByteString t -> HttpHeader AcceptEncoding hValue
            _ -> HttpHeader Other hValue -- Default to Other header for unknown headers

getHeaderValue::HeaderType->[HttpHeader]-> ByteString
getHeaderValue _ [] = throw HeaderTypeNotFoundError
getHeaderValue t (h:rest) =
    case h of
        (HttpHeader httpType httpValue) | httpType == t -> httpValue
        _ -> getHeaderValue t rest

getValueIfHasHeader::HeaderType->[HttpHeader]->(Bool, Maybe ByteString)
getValueIfHasHeader _ [] = (False, Nothing)
getValueIfHasHeader t (h:rest) =
    case h of
        (HttpHeader httpType httpValue) | httpType == t -> (True, Just httpValue)
        _ -> getValueIfHasHeader t rest


lowerByteString::ByteString->ByteString
lowerByteString = BC.map toLower

------------------------------ Exception instances    

data HeaderTypeNotFoundError = HeaderTypeNotFoundError
    deriving (Show)

instance Exception HeaderTypeNotFoundError

------------------------------ Show instances

instance Show HttpRequest where
    show :: HttpRequest -> String
    show (HttpRequest reqStatus reqHeaders reqBody) =
        "HttpRequest { status = " ++ show reqStatus ++
        ", headers = " ++ show reqHeaders ++
        ", body = " ++ show reqBody ++ " }"

instance Show HttpStatus where
    show :: HttpStatus -> String
    show (HttpStatus reqMethod reqPath reqVersion) =
        "HttpStatus { method = " ++ BC.unpack reqMethod ++
        ", path = " ++ BC.unpack reqPath ++
        ", version = " ++ BC.unpack reqVersion ++ " }"

instance Show HttpBody where
    show :: HttpBody -> String
    show (HttpBody reqBody) = "HttpBody " ++ BC.unpack reqBody

instance Show HttpHeader where
    show :: HttpHeader -> String
    show (HttpHeader reqHeaderType reqHeaderValue) =
        "HttpHeader { headerType = " ++ show reqHeaderType ++
        ", headerValue = " ++ BC.unpack reqHeaderValue ++ " }"

instance Show HeaderType where
    show :: HeaderType -> String
    show UserAgent = "UserAgent"
    show Accept = "Accept"
    show AcceptEncoding = "AcceptEncoding"
    show Host = "Host"
    show Connection = "Connection"
    show Other = "Other"