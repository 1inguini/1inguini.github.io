{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hakyll
import Lib
import Lucid

main :: IO ()
main = hakyll $ do
  create ["index.html"] $ do
    route idRoute
    compile $ do
      makeItem $ renderBS (a_ "aaa")
