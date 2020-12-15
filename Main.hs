{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hakyll
import Lucid

import Index
import Template
import qualified Posts2020_12

main :: IO ()
main = hakyllWith defaultConfiguration { destinationDirectory = "docs" } $ do
  create ["index.html"] $ do
    route idRoute
    compile $ do
      makeItem $ renderBS index
      
  create ["post.html"] $ do
    route idRoute
    compile $ do
      makeItem $ renderBS $
        template "2020/12/16" Posts2020_12.date16

