{-# LANGUAGE OverloadedStrings #-}

-- | generate index.html
module Main where

import qualified Data.ByteString.Lazy as BS
import Data.Functor.Identity (Identity (Identity))
import Data.Text (Text, pack)
import Lucid
import Template

main = undefined

index :: [(FilePath, String)] -> Html ()
index links = template "linguiniのブログ" $ do
  section_ $ do
    h1_ "記事"
    ul_ $
      mapM_
        ( \(path, desc) ->
            li_ $ link (pack path) (toHtmlRaw desc)
        )
        links
