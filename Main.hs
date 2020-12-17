{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow
import Control.Exception (Exception (displayException))
import qualified Data.ByteString.Lazy as BS
import Hakyll
import Language.Haskell.Interpreter (OptionVal ((:=)))
import qualified Language.Haskell.Interpreter as Hint
import Lucid
import System.IO (hPutStrLn, stderr)
import Template (BlogPost (..))

posts = "posts/*/*"

main :: IO ()
main =
  hakyllWith defaultConfiguration {destinationDirectory = "docs"} $
    let pathAndFeedConfirguration = "Path&FeedConfiguration"
     in do
          create ["index.html"] $ do
            route idRoute
            compile $ do
              maybeIndex <- unsafeCompiler $ interpret "pages/Index.hs" "index" (Hint.as :: [(FilePath, String)] -> Html ())
              case maybeIndex of
                Left e -> do
                  unsafeCompiler $ hPutStrLn stderr $ displayException e
                  fail "interpret"
                Right index -> do
                  links <-
                    fmap (itemBody >>> second feedTitle)
                      <$> ( loadAllSnapshots posts pathAndFeedConfirguration ::
                              Compiler [Item (FilePath, FeedConfiguration)]
                          )
                  makeItem $
                    renderBS $
                      index links

          match posts $ do
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
                  makeItem $ renderBS $ html blogpost

interpret filepath expr as =
  Hint.runInterpreter $ do
    Hint.set [Hint.languageExtensions := [Hint.OverloadedStrings]]
    Hint.loadModules [filepath]
    Hint.setTopLevelModules ["Main"]
    Hint.setImports ["Prelude", "Lucid", "Template", "Data.Functor.Identity"]
    Hint.setImportsQ [("Data.ByteString.Lazy", Just "BS")]
    Hint.interpret expr as
