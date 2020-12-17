{-# LANGUAGE OverloadedStrings #-}

-- | generate index.html
module Main where

import qualified Data.ByteString.Lazy as BS
import Data.Functor.Identity (Identity (Identity))
import Data.Text (Text, pack)
import Template

main = undefined

index :: IndexData -> Html ()
index indexData = template "linguiniのブログ" $ do
  section_ $ do
    h1_ "記事"
    linkList $ articles indexData
