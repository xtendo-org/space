module Space.Server
    ( run
    , OpenAt(..)
    , parseOpenAt
    ) where

import Space.Import

import qualified Data.ByteString.Char8 as B

import Text.Read (readMaybe)

import Network.HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import Space.UnixSocket
import Space.ModifiedTime

-- OpenAt configuration

data OpenAt
    = OpenAtPort Warp.Port
    | OpenAtUnixSocket FilePath

parseOpenAt :: String -> OpenAt
parseOpenAt s = case readMaybe s :: Maybe Warp.Port of
    Just p  -> OpenAtPort p
    Nothing -> OpenAtUnixSocket s

instance Show OpenAt where
    show (OpenAtPort port) = "port " ++ show port
    show (OpenAtUnixSocket path) = "Unix socket " ++ show path

-- logic

run :: OpenAt -> ByteString -> Wai.Application -> IO ()
run openAt urlPrefix app = do
    B.putStrLn $ mconcat ["Server is opening at ", B.pack (show openAt), ".."]
    case openAt of
        OpenAtPort p -> Warp.runSettings
            (Warp.setPort p Warp.defaultSettings) go
        OpenAtUnixSocket s -> do
            sock <- unixSocket s
            Warp.runSettingsSocket Warp.defaultSettings sock go
  where
    go req respond
        | not (urlPrefix `B.isPrefixOf` url) = respond notFound
        | not (isSafeURL url) = respond notFound
        | (urlPrefix <> "/static/") `B.isPrefixOf` url = serveThis
        | (urlPrefix <> "/asset/") `B.isPrefixOf` url = serveThis
        | otherwise = app appReq respond
      where
        url = Wai.rawPathInfo req
        appReq = req { Wai.rawPathInfo = B.drop (B.length urlPrefix) url }
        serveThis = serve (B.drop (B.length urlPrefix + 1) url) req respond

serve :: ByteString -> Wai.Application
serve path req respond = case mimetype (extension path) of
    Just ctype -> do
        fileMTime <- getMTime path
        respond $ case modifiedSince req fileMTime of
            NotModified -> notModified
            Modified -> Wai.responseFile status200
                [ (hContentType, ctype)
                , (hLastModified, formattedMTime fileMTime)
                ]
                (B.unpack path) Nothing
    Nothing -> respond notFound

-- HTTP responses

notFound :: Wai.Response
notFound = Wai.responseLBS status404
    [(hContentType, "text/plain")]
    "Page not found"

notModified :: Wai.Response
notModified = Wai.responseLBS status304 [] ""

-- utilities

isSafeURL :: ByteString -> Bool
isSafeURL url = and
    [ B.all urlChar url
    , not $ "//" `B.isInfixOf` url
    , not $ "../" `B.isInfixOf` url
    , not $ ".." `B.isSuffixOf` url
    ]

urlChar :: Char -> Bool
urlChar c = or
    [ ord 'a' <= n && n <= ord 'z'
    , ord 'A' <= n && n <= ord 'Z'
    , ord '0' <= n && n <= ord '9'
    , c `B.elem` "-_./"
    ]
  where
    n = ord c

mimetype :: ByteString -> Maybe ByteString
mimetype ext = case ext of
    "jpg"   -> Just "image/jpeg"
    "png"   -> Just "image/png"
    "svg"   -> Just "image/svg+xml"
    "js"    -> Just "application/javascript; charset=utf-8"
    "css"   -> Just "text/css"
    "pdf"   -> Just "application/pdf"
    "eot"   -> Just "application/vnd.ms-fontobject"
    "ttf"   -> Just "application/octet-stream"
    "woff"  -> Just "application/font-woff"
    "woff2" -> Just "application/font-woff2"
    "txt"   -> Just "text/plain"
    "ico"   -> Just "image/vnd.microsoft.icon"
    _       -> Nothing

cExtensionLongest :: Int
cExtensionLongest = 5

extension :: ByteString -> ByteString
extension path = snd $ B.breakEnd (== '.') $
    B.drop (B.length path - cExtensionLongest) path
