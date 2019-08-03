module Space.Template
    ( buildHtml
    , buildText
    , LT.fromText
    , LT.decimal
    ) where

import Space.Import

-- extra modules

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LT
import qualified Data.Text.Lazy.Builder.Int as LT
import Lucid

buildHtml :: [LT.Builder] -> Html ()
buildHtml = toHtml . buildText

buildText :: [LT.Builder] -> Text
buildText = LT.toStrict . LT.toLazyText . mconcat
