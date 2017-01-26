{-# language TemplateHaskell #-}

module SimpleFW.Asset where

import SimpleFW.Import

-- extra modules

import qualified Data.ByteString.RawFilePath as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (qAddDependentFile)
import System.RawFilePath

-- local modules

import SimpleFW.Hash

preprocessFile
    :: (RawFilePath -> RawFilePath -> IO ())
    -> RawFilePath
    -> RawFilePath
    -> Q [Dec]
preprocessFile cmd srcPath rawDstPath = do
    qAddDependentFile $ T.unpack $ T.decodeUtf8 srcPath
    hashed <- runIO $ B.take 16 . hash <$> B.readFile srcPath
    let
        dstNameWithHash = dstName <> "." <> hashed <> "." <> dstExt
        dstPath = dstDir <> dstNameWithHash
    runIO $ doesFileExist dstPath >>= \e -> unless e $ cmd srcPath dstPath
    let
        assetName = mkName $ T.unpack $ T.decodeUtf8 dstName
        assetValue = T.unpack $ T.decodeUtf8 dstNameWithHash
    return
        [ SigD assetName (ConT ''RawFilePath)
        , ValD (VarP assetName) (NormalB $ LitE $ StringL assetValue) []
        ]
  where
    (dstDir, dstName) = B.breakEnd (== (fromIntegral $ ord '/')) $
        B.init dstChunk
    (dstChunk, dstExt) = B.breakEnd (== (fromIntegral $ ord '.')) rawDstPath
