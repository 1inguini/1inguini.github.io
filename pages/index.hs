-- | generate index.html
module Main where

import Share
import Template

main = undefined

index :: Webpage IndexProtocol
index =
  webpageCommon . articleCommon
    . set titleL "Index"
    . set descriptionL "トップページ"
    . set modifiedDatesL ["2020-12-16"]
    . set webpageBodyL indexBody
    $ def

indexBody :: WebpageBody IndexProtocol ()
indexBody = do
  indexData <- view (typed @(IndexProtocol False)) <$> ask
  section "外部リンク" $
    hyperlinkList $ externals indexData

  section "記事" $
    hyperlinkList $ articles indexData
