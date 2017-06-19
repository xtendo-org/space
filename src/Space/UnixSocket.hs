module Space.UnixSocket
    ( Socket
    , unixSocket
    ) where

import Space.Import

import Network.Socket
import System.Directory

unixSocket :: FilePath -> IO Socket
unixSocket path = do
    tryRemoveFile path
    s <- socket AF_UNIX Stream defaultProtocol
    bind s (SockAddrUnix path)
    listen s maxListenQueue
    return s

tryRemoveFile :: FilePath -> IO ()
tryRemoveFile path = catchIOError (removeFile path) (const (return ()))
