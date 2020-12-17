{-# LANGUAGE OverloadedStrings #-}

-- | post of 2020/12/16
module Main where

import Template

main = undefined

-- post :: BlogPost
post = mkBlogPost
  defaultFeedConfig
    { feedTitle = "ブログ準備中その0",
      feedDescription = "HakyllとLucidでブログ書き始めたよ"
    }
  $ do
    section_ $ do
      h2_ "きっかけ"
      p_ $
        "ことの発端は" <> link "https://linguini.booth.pm/items/2496957" "リア-アリス向けロングコート" <> "のライセンスを公開するために作った" <> githubLink "1inguini/Licences" <> "をみていたときに、「" <> obviousLink "https://1inguini.github.io/Licences/RearAliceCoatVN3license.pdf" <> "を公開してるのに" <> obviousLink "https://1inguini.github.io/Licences" <> "に行こうとすると404になるの納得いかないな」と思い立ってindex.html書き始めたことでして。"
      p_ $
        "index.htmlを書いているうちに(vscodeの補完が優秀だったこともあり)html書くの楽しいなと思ったものの" <> obviousLink "https://1inguini.github.io/Licences" <> "に書く内容は大して無いのですぐに物足りなくなったわけで。"
      p_ $
        "気付いたら" <> link "http://jaspervdj.be/hakyll" "Hakyll" <> "と" <> link "https://hackage.haskell.org/package/lucid" "Lucid" <> "でブログを書こうとしていました。"

    section_ $ do
      h2_ "今までの進捗"
      p_ $
        "Hakyllは生成したファイルを_site/以下に吐くのでgitのルートディレクトリか/docディレクトリかにサイトがあることを期待するGitHub Pagesに使うには少し工夫が必要でした。" <> "何したかって言いますとgh-pagesブランチを作ってそれを" <> code_ "git submodule add -b gh-pages https://github.com/1inguini/1inguini.github.io.git _site" <> "しました。" <> "「しました」ってさらっと書いてますが" <> code_ "stack run rebuild" <> "して.gitディレクトリを消し飛ばしたり" <> code_ "git reset --hard" <> code_ "git push -f" <> "しまくったりと(良い子は真似しないでね!)すんなりはできませんでした(もっとgitと仲良くなろうね)。" <> "ところで今見たらHakyll側で割と簡単に出力先を変えれるっぽいな?"

      p_ $
        "というわけで早速" <> code_ "main = hakyll $ do" <> "を" <> code_ "hakyllWith defaultConfiguration { destinationDirectory = \"doc\" } $ do" <> "に置き換えました。いやー快適。やったね。"

    section_ $ do
      h2_ "追記"
      p_ $
        "docじゃなくてdocsだったわウケ。「No " <> code_ "/docs" <> " folder was found to build GitHub Pages. Check the source setting for this repository.」って怒られちゃった。"
