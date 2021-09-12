module Main where

import qualified Data.Aeson as Aeson
import Data.Binary (decode)
import Hakyll
import Language.Haskell.Interpreter (OptionVal ((:=)))
import qualified Language.Haskell.Interpreter as Hint
import Lucid (Html, renderBS)
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Char as C
import RIO.FilePath
import qualified RIO.List as L
import qualified RIO.Process as Proc (byteStringInput, readProcess_, setStdin)
import RIO.State
import qualified RIO.Text as T
import qualified RIO.Text.Lazy as Text
import Share
import System.IO (hPutStrLn)

postsDir, postsHaskell, htmls, commentsRoot, comments :: FilePath
postsDir = "posts/*/*"
htmls = "**.html"
commentsRoot = "comments"
comments = commentsRoot </> "**.json"
postsHaskell = postsDir </> "index.hs"

indexDataWithExternals :: IndexProtocol False
indexDataWithExternals =
  def
    { externals =
        fmap
          (first https)
          [ ("twitter.com/1inguini", "Twitter"),
            ("twitter.com/1inguini1tasita", "Twitterの飲精アカウント"),
            ("github.com/1inguini", "GitHub"),
            ("linguini.booth.pm", "BOOTH"),
            ("www.amazon.co.jp/hz/wishlist/dl/invite/3xNE72T?ref_=wl_share", "干し芋(amazon)"),
            ("my.aliexpress.com/wishlist/wish_list_product_list.htm?spm=a2g0s.8937460.0.0.643c2e0eb67CrL&currentGroupId=1000000008064076", "干しいも(AliExpress)"),
            ("vrchat.com/home/user/usr_7be90808-2858-4707-b1b9-b2b5636ba686", "VRChat")
          ]
    }

-- -- camelToSnake ::
-- camelToSnake :: String -> String
-- camelToSnake (c : cs) =
--   let upperToUnderscore c
--         | C.isUpper c = ('_' :) . (C.toLower c :)
--         | otherwise = (c :)
--    in C.toLower c : foldr upperToUnderscore "" cs

feedContext ::
  WebpageHakyllDataExchangeProtocol protocol =>
  Webpage protocol ->
  Context String
feedContext page =
  let fromMaybeEmptyModifiedDates safeGetElement =
        fromMaybe "Error: modifiedDates empty" $
          safeGetElement $ view modifiedDatesL page
   in mconcat
        [ constField "title" . T.unpack $ view titleL page,
          constField "description" . T.unpack $ view descriptionL page,
          constField "published" . T.unpack $
            fromMaybeEmptyModifiedDates L.headMaybe,
          constField "updated" . T.unpack $
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

loadBodies = (fmap itemBody <$>) . loadAll

setFileContents ::
  (HasFileContentsRequest req, HasFileContentsResponse res) =>
  (req -> Compiler (res -> res))
setFileContents hasRequests = do
  postDir <- takeDirectory <$> getResourceFilePath
  contents <-
    mapM loadBody $
      fromFilePath . (postDir </>)
        <$> view fileRequestsL hasRequests
  pure $ set fileContentsL contents

populateCommon ::
  (HasFromWebpageCommon fromWebpage, HasToWebpageCommon toWebpage) =>
  (fromWebpage -> toWebpage -> Compiler toWebpage)
populateCommon fromWebpage toWebpage = (`execStateT` toWebpage) $ do
  path <-
    lift $
      fromMaybe (fail "failed at getting destination filepath")
        <$> (getRoute =<< getUnderlying)
  modify $ set pathL path

  lsOfMayComm <-
    lift
      ( fmap (Aeson.decode :: BL.ByteString -> Maybe Comment)
          <$> loadBodies
            ( fromGlob $
                commentsRoot
                  </> (\case '.' : p -> p; p -> p) (takeDirectory path)
                  <> "*.json"
            )
      )
  modify . set commentsL =<< case lsOfMayComm of
    [] -> pure []
    _ ->
      lift $
        maybe (fail "failed at decoding comments") pure $
          sequenceA lsOfMayComm

  modify =<< lift (setFileContents fromWebpage)

main :: IO ()
main =
  hakyllWith defaultConfiguration {destinationDirectory = "docs"} $
    let pathAndWebpageData = "#0"
     in do
          match "pages/index.hs" $ do
            route $ constRoute "index.html"
            compile $ do
              articles <-
                fmap (itemBody >>> second (view titleL))
                  <$> ( loadAllSnapshots (fromGlob postsHaskell) pathAndWebpageData ::
                          Compiler [Item (FilePath, CommonProtocol True)]
                      )
              index <- interpret "index" (Hint.as :: Webpage IndexProtocol)
              populateCommon index indexDataWithExternals {articles = articles}
                >>= webpageCompiler (view webpageBodyL index)

          match (fromGlob postsHaskell) $ do
            route $ setExtension "html"
            compile $ do
              (Just path) <- getRoute =<< getUnderlying
              blogPost <- interpret "post" (Hint.as :: Webpage ArticleProtocol)
              makeItem (path, view webpageCommonDataL blogPost)
                >>= saveSnapshot pathAndWebpageData

              populateCommon blogPost (def :: ArticleProtocol False)
                >>= webpageCompiler (view webpageBodyL blogPost)

          match (fromGlob htmls) $ compile getResourceLBS

          match (fromGlob comments) $ compile getResourceLBS

webpageCompiler ::
  WebpageHakyllDataExchangeProtocol protocol =>
  WebpageBody protocol () ->
  protocol False ->
  Compiler (Item BL.ByteString)
webpageCompiler webpageBody envFromHakyll =
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
        Hint.set
          [ Hint.languageExtensions
              := [ Hint.DataKinds,
                   Hint.DefaultSignatures,
                   Hint.DeriveGeneric,
                   Hint.ExistentialQuantification,
                   Hint.FlexibleContexts,
                   Hint.FlexibleInstances,
                   Hint.DuplicateRecordFields,
                   Hint.GADTs,
                   Hint.GeneralizedNewtypeDeriving,
                   Hint.LambdaCase,
                   Hint.MultiParamTypeClasses,
                   Hint.OverloadedStrings,
                   Hint.ScopedTypeVariables,
                   Hint.StandaloneDeriving,
                   Hint.TypeFamilies,
                   Hint.TypeApplications,
                   Hint.NoImplicitPrelude
                 ]
          ]
        Hint.loadModules [filepath, "templates/Template.hs"]
        Hint.setTopLevelModules ["Main"]
        Hint.setImports ["Share", "Template"]
        Hint.interpret expr as
