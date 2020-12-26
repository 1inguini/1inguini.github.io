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
    . set titleL "ブログ準備中その0"
    . set descriptionL "HakyllとLucidでブログ書き始めたよ"
    . set tagsL ["Hakyll", "Website"]
    . set modifiedDatesL ["2020-12-16"]
    . set webpageBodyL postBody
    $ def

postBody :: WebpageBody ArticleProtocol ()
postBody = do
  section "きっかけ" $ do
    paragraph $
      "ことの発端は" <> hyperlink "https://linguini.booth.pm/items/2496957" "リア-アリス向けロングコート" <> "のライセンスを公開するために作った" <> hyperlinkGitHub "1inguini/Licences" <> "をみていたときに、「" <> hyperlinkEcho "https://1inguini.github.io/Licences/RearAliceCoatVN3license.pdf" <> "を公開してるのに" <> hyperlinkEcho "https://1inguini.github.io/Licences" <> "に行こうとすると404になるの納得いかないな」と思い立ってindex.html書き始めたことでして。"
    paragraph $
      "index.htmlを書いているうちに(vscodeの補完が優秀だったこともあり)html書くの楽しいなと思ったものの" <> hyperlinkEcho "https://1inguini.github.io/Licences" <> "に書く内容は大して無いのですぐに物足りなくなったわけで。"
    paragraph $
      "気付いたら" <> hyperlink "http://jaspervdj.be/hakyll" "Hakyll" <> "と" <> hyperlink "https://hackage.haskell.org/package/lucid" "Lucid" <> "でブログを書こうとしていました。" ::
      WebpageBody ArticleProtocol ()

  section "今までの進捗" $ do
    paragraph $
      "Hakyllは生成したファイルを_site/以下に吐くのでgitのルートディレクトリか/docディレクトリかにサイトがあることを期待するGitHub Pagesに使うには少し工夫が必要でした。" <> "何したかって言いますとgh-pagesブランチを作ってそれを" <> code "git submodule add -b gh-pages https://github.com/1inguini/1inguini.github.io.git _site" <> "しました。" <> "「しました」ってさらっと書いてますが" <> code "stack run rebuild" <> "して.gitディレクトリを消し飛ばしたり" <> code "git reset --hard" <> code "git push -f" <> "しまくったりと(良い子は真似しないでね!)すんなりはできませんでした(もっとgitと仲良くなろうね)。"
        <> "ところで今見たらHakyll側で割と簡単に出力先を変えれるっぽいな?" ::
      WebpageBody ArticleProtocol ()

    paragraph $
      "というわけで早速" <> code "main = hakyll $ do" <> "を" <> code "hakyllWith defaultConfiguration { destinationDirectory = \"doc\" } $ do" <> "に置き換えました。いやー快適。やったね。" ::
      WebpageBody ArticleProtocol ()

  section "追記" $ do
    paragraph $
      "docじゃなくてdocsだったわウケ。「No " <> code "/docs" <> " folder was found to build GitHub Pages. Check the source setting for this repository.」って怒られちゃった。"
