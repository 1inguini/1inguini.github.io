{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow
import Control.Exception (Exception (displayException))
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.String (IsString)
import qualified Data.Text.Lazy as Text
import Hakyll
import Language.Haskell.Interpreter (OptionVal ((:=)))
import qualified Language.Haskell.Interpreter as Hint
import Lucid
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess)
import Template (BlogPost (..), IndexData (..), Link, https)

postDir = "posts/*/*"

redirectsDir :: IsString s => s
redirectsDir = "redirects"

defaultIndexData :: IndexData
defaultIndexData =
  IndexData
    { externals =
        [ (https "twitter.com/1inguini", "Twitter"),
          (https "twitter.com/1inguini1tasita", "Twitterの飲精アカウント"),
          (https "github.com/1inguini", "GitHub"),
          (https "linguini.booth.pm", "BOOTH"),
          (https "www.amazon.co.jp/hz/wishlist/dl/invite/ghyjmBH?ref_=wl_share", "干し芋"),
          (https "vrchat.com/home/user/usr_7be90808-2858-4707-b1b9-b2b5636ba686", "VRChat")
        ],
      articles = []
    }

main :: IO ()
main =
  hakyllWith defaultConfiguration {destinationDirectory = "docs"} $
    let pathAndFeedConfirguration = "Path&FeedConfiguration"
     in do
          create ["index.html"] $ do
            route idRoute
            compile $ do
              maybeIndex <- unsafeCompiler $ interpret "pages/Index.hs" "index" (Hint.as :: IndexData -> Html ())
              case maybeIndex of
                Left e -> do
                  unsafeCompiler $ hPutStrLn stderr $ displayException e
                  fail "interpret"
                Right index -> do
                  articles <-
                    fmap (itemBody >>> second feedTitle)
                      <$> ( loadAllSnapshots postDir pathAndFeedConfirguration ::
                              Compiler [Item (FilePath, FeedConfiguration)]
                          )
                  makeHtml $
                    index defaultIndexData {articles = articles}

          match postDir $ do
            route $ setExtension "html"
            compile $ do
              src <- getResourceFilePath
              (Just path) <- getRoute =<< getUnderlying
              result <- unsafeCompiler $ interpret src "post" (Hint.as :: BlogPost)
              case result of
                Left e -> do
                  unsafeCompiler $ hPutStrLn stderr $ displayException e
                  fail "interpret"
                Right blogpost -> do
                  makeItem (path, feedConfig blogpost)
                    >>= saveSnapshot pathAndFeedConfirguration
                  makeHtml $ html blogpost

makeHtml :: Html () -> Compiler (Item String)
makeHtml html =
  unsafeCompiler
    ( readProcess
        "npx"
        ["js-beautify", "--type=html", "-"]
        (Text.unpack $ renderText html)
    )
    >>= makeItem

interpret filepath expr as =
  Hint.runInterpreter $ do
    Hint.loadModules [filepath]
    Hint.setTopLevelModules ["Main"]
    Hint.setImports ["Prelude", "Lucid", "Template"]
    Hint.setImportsQ [("Data.ByteString.Lazy", Just "BS")]
    Hint.interpret expr as
