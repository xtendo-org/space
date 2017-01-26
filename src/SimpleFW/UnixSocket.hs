module SimpleFW.UnixSocket
    ( Socket
    , unixSocket
    ) where

import Network.Socket
import System.RawFilePath

import qualified Data.ByteString.Char8 as B

unixSocket :: RawFilePath -> IO Socket
unixSocket path = do
    tryRemoveFile path
    s <- socket AF_UNIX Stream defaultProtocol
    bind s (SockAddrUnix (B.unpack path))
    listen s maxListenQueue
    return s
