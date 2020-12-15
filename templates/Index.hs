{-# LANGUAGE OverloadedStrings #-}

-- | generate index.html
module Index where

import Lucid

import Template

index :: Html ()
index = template "linguiniのブログ" $ do
  section_ $ do
    h1_ "まだ何もない"
    article_ "ああああああ"
