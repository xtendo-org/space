module SimpleFW.View
    ( ByteString
    , module Network.HTTP.Types
    , module Network.Wai
    , Html
    , render
    , json
    , jsonCType
    , notFound
    , methodNotAllowed
    , intArg
    ) where

import qualified Data.Aeson as Aeson
import Data.Attoparsec.ByteString (parseOnly)
import Data.Attoparsec.ByteString.Char8 (decimal)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as B
import Network.HTTP.Types
import Network.Wai
    ( Request
    , Response
    , responseLBS
    , responseBuilder
    , requestMethod
    , requestHeaders
    )
import Lucid (Html, renderBS)

render :: Status -> ResponseHeaders -> Html a -> Response
render s h c = responseLBS s (ctype : h) (renderBS c)
  where
    ctype = (hContentType, "text/html; charset=utf-8")

json :: Aeson.ToJSON x => x -> Response
json = responseBuilder status200 [jsonCType] .
    Aeson.fromEncoding . Aeson.toEncoding

-- utility

notFound :: Response
notFound = responseBuilder status404 [] "Not Found"

methodNotAllowed :: Response
methodNotAllowed = render status405 [] "Method Not Allowed"

jsonCType :: Header
jsonCType = (hContentType, "application/json; charset=utf-8")

intArg :: Integral int => ByteString -> (int -> IO Response) -> IO Response
intArg rawArg handler = case parseOnly decimal rawArg of
    Left _ -> return $ responseBuilder status400 [jsonCType] msg
    Right i -> handler i
  where
    msg = mconcat
        [ "{\"message\":\"could not parse "
        , B.byteString rawArg
        ," as an integral value\"}"
        ]
