{-# LANGUAGE OverloadedStrings #-}

-- | post of 2020/12/16
module Main where

import Share
import Template

main = undefined

-- post :: BlogPost
post :: Webpage ArticleProtocol
post =
  webpageCommon . articleCommon
    . set titleL "ブログ生成にHintつかいはじめました&Hakyll.loadAllSnapShotsで嵌まった"
    . set descriptionL "Lucidからhtmlを生成するときにHint(Runtime Haskell interpreter)使うようにして新しい記事を作るたびにHakyllをビルドしなくてよくなった。Hakyll.loadAllSnapShotsは型がわからないと動かないので使ってるところを書く前はエラーが出るが型注釈を書けばエラーは消える"
    . set modifiedDatesL ["2020-12-17"]
    . set webpageBodyL postBody
    $ def

postBody :: WebpageBody ArticleProtocol ()
postBody = do
  section "仮のTL;DR" $
    paragraph
      "Lucidからhtmlを生成するときにHint(Runtime Haskell interpreter)使うようにして新しい記事を作るたびにHakyllをビルドしなくてよくなった。Hakyll.loadAllSnapShotsは型がわからないと動かないので使ってるところを書く前はエラーが出るが型注釈を書けばエラーは消える"

-- iframe_
-- [src_ "snippet.html"]
-- ""
