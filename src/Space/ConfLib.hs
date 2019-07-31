-- This module provides the function "derive" which takes a data type name. It
-- will derive the instances of FromJSON and ToJSON, along with a function
-- "decode" which takes a RawFilePath of a YAML file and decodes it to a data
-- value of the aforementioned type.
--
-- In other words,
--
-- 1. Define a Haskell data type for config.
-- 2. Use the "derive" function.
-- 3. Now you have a decode function that will read a YAML file and create a
-- config value of your type.

{-# language TemplateHaskell #-}

module Space.ConfLib
    ( derive
    ) where

-- base modules

import Space.Import

-- extra modules

import qualified Data.Yaml as Y
import Language.Haskell.TH
import Data.Aeson.TH

-- local modules

import qualified Data.ByteString.RawFilePath as B

derive :: Name -> Q [Dec]
derive d = do
    decs <- deriveJSON defaultOptions { fieldLabelModifier = lowerSnake } d
    decodeDecs <- [d|
        openConf :: RawFilePath -> IO $(return $ ConT d)
        openConf path = Y.decodeEither' <$> B.readFile path
          >>= either (die .show) return
        |]
    return $ decs ++ decodeDecs
  where
    lowerSnake :: String -> String
    lowerSnake [] = []
    lowerSnake (x : xs)
        | isUpper x = '_' : toLower x : lowerSnake xs
        | otherwise = x : lowerSnake xs

