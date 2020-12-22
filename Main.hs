{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Hakyll
import Language.Haskell.Interpreter (OptionVal ((:=)))
import qualified Language.Haskell.Interpreter as Hint
import Lucid (Html, renderBS)
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Char as C
import qualified RIO.List as L
import qualified RIO.Process as Proc (byteStringInput, readProcess_, setStdin)
import qualified RIO.Text.Lazy as Text
import System.IO (hPutStrLn)
import Template

postsDir = "posts/*/"

postsHaskell = fromGlob $ postsDir <> "*.hs"

postsHtml = fromGlob $ postsDir <> "*.html"

redirectsDir :: IsString s => s
redirectsDir = "redirects"

defaultIndexData :: IndexData
defaultIndexData =
  IndexData
    { externals =
        fmap
          (first https)
          [ ("twitter.com/1inguini", "Twitter"),
            ("twitter.com/1inguini1tasita", "Twitterの飲精アカウント"),
            ("github.com/1inguini", "GitHub"),
            ("linguini.booth.pm", "BOOTH"),
            ("www.amazon.co.jp/hz/wishlist/dl/invite/ieqolZ4?ref_=wl_share", "干し芋"),
            ("vrchat.com/home/user/usr_7be90808-2858-4707-b1b9-b2b5636ba686", "VRChat")
          ],
      articles = []
    }

-- camelToSnake ::
camelToSnake :: String -> String
camelToSnake (c : cs) =
  let upperToUnderscore c
        | C.isUpper c = ('_' :) . (C.toLower c :)
        | otherwise = (c :)
   in C.toLower c : foldr upperToUnderscore "" cs

feedContext :: Webpage fromHakyll -> Context String
feedContext page =
  let fromMaybeEmptyModifiedDates safeGetElement =
        fromMaybe "Error: modifiedDates empty" $
          safeGetElement $ view modifiedDatesL page
   in mconcat
        [ constField "title" $ view titleL page,
          constField "description" $ view descriptionL page,
          constField "published" $
            fromMaybeEmptyModifiedDates L.headMaybe,
          constField "updated" $
            fromMaybeEmptyModifiedDates L.lastMaybe,
          -- constField "body" $ body page,
          defaultContext
        ]

-- rss =
--   hakyll $ do
--     create ["1900-01-01.html"] $ do
--       route idRoute
--       compile $ makeItem ("test index.html" :: String)
--     create ["rss"] $ do
--       route idRoute
--       compile $ do
--         item <- makeItem "hello"
--         index <- load "1900-01-01.html"
--         renderRss defaultFeedConfig (defaultContext <> constField "description" "hoge") [index]

main :: IO ()
main =
  hakyllWith defaultConfiguration {destinationDirectory = "docs"} $
    let pathAndWebpageData = "#0"
     in do
          match "pages/Index.hs" $ do
            route $ constRoute "index.html"
            compile $ do
              articles <-
                fmap (itemBody >>> second (view titleL))
                  <$> ( loadAllSnapshots postsHaskell pathAndWebpageData ::
                          Compiler [Item (FilePath, WebpageData)]
                      )
              index <- interpret "index" (Hint.as :: Webpage IndexData)
              webpageCompiler
                FromHakyll
                  { fileContents = mempty,
                    pageTypeSpecific = defaultIndexData {articles = articles}
                  }
                $ body index

          match postsHaskell $ do
            route $ setExtension "html"
            compile $ do
              (Just path) <- getRoute =<< getUnderlying
              blogPost <- interpret "post" (Hint.as :: Webpage ArticleData)
              makeItem (path, webpageData blogPost)
                >>= saveSnapshot pathAndWebpageData
              requestedFiles <-
                mapM
                  (loadBody . fromFilePath :: FilePath -> Compiler String)
                  $ view hasRequestL blogPost
              webpageCompiler
                FromHakyll
                  { fileContents = requestedFiles,
                    pageTypeSpecific = ArticleData
                  }
                $ body blogPost

          match "**/*.html" $ do
            route idRoute
            compile getResourceBody

webpageCompiler :: FromHakyll fromHakyll -> WebpageBody fromHakyll () -> Compiler (Item BL.ByteString)
webpageCompiler envFromHakyll webpageBody =
  unsafeCompiler
    ( Proc.readProcess_ $
        Proc.setStdin
          ( Proc.byteStringInput $
              renderWebpageBody
                (mkDefaultWebpageEnv envFromHakyll)
                webpageBody
          )
          "npx js-beautify --type=html -"
    )
    >>= (fst >>> makeItem)

-- may fail
interpret :: Typeable a => String -> a -> Compiler a
interpret code as = do
  filePath <- getResourceFilePath
  eitherResult <- unsafeCompiler $ interpret' filePath code as
  case eitherResult of
    Left e -> fail $ displayException e
    Right result -> pure result
  where
    interpret' :: Typeable a => FilePath -> String -> a -> IO (Either Hint.InterpreterError a)
    interpret' filepath expr as =
      Hint.runInterpreter $ do
        Hint.loadModules [filepath]
        Hint.setTopLevelModules ["Main"]
        Hint.setImports ["RIO", "Template"]
        Hint.interpret expr as
