{-# LANGUAGE OverloadedStrings #-}

-- | posts of 2020/12/16
module Main where

import Template

main = undefined

post :: BlogPost
post = mkBlogPost
  defaultFeedConfig
    { feedTitle = "ブログ生成にHintつかいはじめました&Hakyll.loadAllSnapShotsで嵌まった",
      feedDescription = "Lucidからhtmlを生成するときにHint(Runtime Haskell interpreter)使うようにして新しい記事を作るたびにHakyllをビルドしなくてよくなった。Hakyll.loadAllSnapShotsは型がわからないと動かないので使ってるところを書く前はエラーが出るが型注釈を書けばエラーは消える"
    }
  $ do
    section_ $ do
      h2_ "仮のTL;DR"
      p_ "Lucidからhtmlを生成するときにHint(Runtime Haskell interpreter)使うようにして新しい記事を作るたびにHakyllをビルドしなくてよくなった。Hakyll.loadAllSnapShotsは型がわからないと動かないので使ってるところを書く前はエラーが出るが型注釈を書けばエラーは消える"
