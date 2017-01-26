module SimpleFW.Hash
    ( hash
    ) where

import SimpleFW.Import

import qualified Data.ByteArray as BA
import qualified Data.ByteString.Base64.URL as B64
import qualified Crypto.Hash as Hash
import Crypto.Hash.Algorithms (Keccak_256)

hash :: ByteString -> ByteString
hash b = B64.encode $ BA.convert (Hash.hash b :: Hash.Digest Keccak_256)
