{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hakyll
import Lucid

import Index

main :: IO ()
main = hakyll $ do
  create ["index.html"] $ do
    route idRoute
    compile $ do
      makeItem $ renderBS index
