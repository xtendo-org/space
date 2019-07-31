-- The import statements that are used so often that it's better to have a
-- module for importing all of them. Including the data types (ByteString,
-- Text, etc.) and the harmless base modules (Control.Monad, System.IO, etc.).
-- Intended to be imported globally across all modules.

module Space.Import
    ( module Module

    , Builder
    , ByteString
    , RawFilePath
    , Text

    ) where

-- base modules

import Control.Applicative as Module
import Control.Concurrent.STM as Module
import Control.Exception as Module
import Control.Monad as Module
import Data.Char as Module
import Data.Either as Module
import Data.Foldable as Module
import Data.Int as Module
import Data.List as Module
import Data.Maybe as Module
import Data.Monoid as Module
import Data.Word as Module
import System.Exit as Module
import System.IO as Module
import System.IO.Error as Module

-- extra data types

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.RawFilePath (RawFilePath)
import Data.Text (Text)

-- import Time.Types as Module
