{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | basic templates
module Template
  ( module Hakyll,
    module Data.Functor.Identity,
    HasAnnotationIndex (..),
    HasDescription (..),
    HasFileContentsRequest (..),
    HasFileContentsResponse (..),
    HasHeaderLevel (..),
    HasModifiedDates (..),
    HasTags (..),
    HasTitle (..),
    HasWebpageBody (..),
    Webpage (..),
    WebpageBody (..),
    WebpageEnv (..),
    Link (..),
    hyperlink,
    hyperlinkEcho,
    hyperlinkGitHub,
    hyperlinkInternal,
    hyperlinkList,
    articleCommon,
    code,
    defaultFeedConfig,
    mkDefaultWebpageEnv,
    header,
    https,
    link_,
    meta_,
    newTabAttr,
    paragraph,
    renderWebpageBody,
    section,
    toWebpageBody,
    toWebpageBodyRaw,
    webpageCommon,
  )
where

-- Contex StringはStringのレコード(Map)みたいなもの!(暴論)

import Data.Binary
import Data.Functor.Identity
import Data.Generics.Product
import Data.Kind (Constraint)
import Data.String (IsString)
import Data.Typeable
import GHC.Base (Symbol)
import Hakyll (FeedConfiguration (..))
import Lucid hiding (doctypehtml_, link_, meta_, nk_)
import qualified Lucid (doctypehtml_, link_, meta_)
import qualified Lucid.Base as Lucid (makeAttribute)
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.List as L
import RIO.State
import qualified RIO.Text as T
import RIO.Time

class HasTitle a where
  titleL :: Lens' a String
  default titleL :: HasField' "title" a String => Lens' a String
  titleL = field' @"title"

class HasDescription a where
  descriptionL :: Lens' a String
  default descriptionL :: HasField' "description" a String => Lens' a String
  descriptionL = field' @"description"

class HasModifiedDates a where
  modifiedDatesL :: Lens' a [String]
  default modifiedDatesL :: HasField' "modifiedDates" a [String] => Lens' a [String]
  modifiedDatesL = field' @"modifiedDates"

class HasWebpageBody (pageType :: WebpageType) a where
  webpageBodyL :: Lens' a (WebpageBody pageType ())
  default webpageBodyL :: HasField' "webpageBody" a (WebpageBody pageType ()) => Lens' a (WebpageBody pageType ())
  webpageBodyL = field' @"webpageBody"

class HasWebpageData a where
  webpageDataL :: Lens' a (ToWebpage x)
  default webpageDataL :: HasField' "webpageData" a (ToWebpage x) => Lens' a (ToWebpage x)
  webpageDataL = field' @"webpageData"

class HasHeaderLevel env where
  headerLevelL :: Lens' env Int
  default headerLevelL :: HasField' "headerLevel" env Int => Lens' env Int
  headerLevelL = field' @"headerLevel"

class HasAnnotationIndex env where
  annotationIndexL :: Lens' env Int
  default annotationIndexL :: HasField' "annotationIndex" env Int => Lens' env Int
  annotationIndexL = field' @"annotationIndex"

class HasFileContentsRequest hasRequest where
  fileRequestsL :: Lens' hasRequest [FilePath]
  default fileRequestsL :: HasField' "fileRequests" hasRequest [FilePath] => Lens' hasRequest [FilePath]
  fileRequestsL = field' @"fileRequests"

class HasFileContentsResponse hasResponse where
  fileContentsL :: Lens' hasResponse [String]
  default fileContentsL :: HasField' "fileContents" hasResponse [String] => Lens' hasResponse [String]
  fileContentsL = field' @"fileContents"

class HasTags a where
  tagsL :: Lens' a [String]
  default tagsL :: HasField' "tags" a [String] => Lens' a [String]
  tagsL = field' @"tags"

data WebpageType
  = ArticlePage
  | IndexPage
  deriving (Eq, Show, Generic)

data Webpage (pageType :: WebpageType) = Webpage
  { webpageData :: FromWebpage pageType,
    webpageBody :: WebpageBody pageType ()
  }
  deriving (Generic)

instance HasTitle (Webpage ArticlePage) where
  titleL = field' @"webpageData" % titleL

instance HasTitle (Webpage IndexPage) where
  titleL = field' @"webpageData" % titleL

instance HasWebpageBody pageType (Webpage pageType)

-- -- data ExampleFileContentRequests a = Example {codeSnippet0, codeSnippet1 :: a} deriving (Functor)といった型を定義してExampleFileContentRequests FilePathで内容がほしいファイルを指定、HakyllがExampleFileContentRequests String(このStringはファイルの中身)といった形で返してくれることを期待する
-- -- Functional DependenciesがないとfileContentRequestsL(もしくはfileContentsL)だけ見たときにHasFileContentsRequestResponse AProtocol ARequest AResponseの可能性だけでなくHasFileContentsRequestResponse AProtocol ARequest BOtherResponseSuchAsIntの可能性も出てきてしまうのでエラーが出る、これブログに書こう
-- class
--   Functor protocol =>
--   HasFileContentsRequestResponse protocol hasRequest hasResponse
--     | protocol -> hasRequest hasResponse
--   where
--   fileContentsRequestL :: Lens' hasRequests (protocol FilePath)
--   fileContentsL :: Lens' hasResposes (protocol String)

newtype WebpageBody pageType a = WebpageBody {runWebpageBody :: HtmlT (Reader (WebpageEnv pageType)) a}
  deriving (Generic, Functor, Applicative, Monad, MonadReader (WebpageEnv pageType))

deriving instance Semigroup (WebpageBody pageType ())

instance Default (WebpageEnv pageType) => Show (WebpageBody pageType ()) where
  show = show . renderWebpageBody def

instance Default (Webpage ArticlePage) where
  def =
    Webpage
      { webpageData = def,
        webpageBody = WebpageBody mempty
      }

instance Default (Webpage IndexPage) where
  def =
    Webpage
      { webpageData = def,
        webpageBody = WebpageBody mempty
      }

instance IsString (WebpageBody pageType ()) where
  fromString = WebpageBody . toHtmlRaw

instance (f ~ WebpageBody pageType a) => Term [Attribute] (f -> WebpageBody pageType a) where
  termWith name f attr = WebpageBody . termWith name f attr . runWebpageBody

instance Term (WebpageBody pageType a) (WebpageBody pageType a) where
  termWith name f = WebpageBody . termWith name f . runWebpageBody

toWebpageBody :: ToHtml a => a -> WebpageBody pageType ()
toWebpageBody = WebpageBody . toHtml

toWebpageBodyRaw :: ToHtml a => a -> WebpageBody pageType ()
toWebpageBodyRaw = WebpageBody . toHtmlRaw

renderWebpageBody :: WebpageEnv pageType -> WebpageBody pageType () -> BL.ByteString
renderWebpageBody env body =
  runWebpageBody body
    & renderBST
    & (`runReader` env)

class WebpageHakyllDataExchangeProtocol (pageType :: WebpageType) where
  data FromWebpage pageType :: *
  data ToWebpage pageType :: *

-- data WebpageData = WebpageData
--   { title :: String,
--     description :: String,
--     modifiedDates :: [String] -- older first iso8601
--   }
--   deriving (Show, Generic)

instance WebpageHakyllDataExchangeProtocol ArticlePage where
  data FromWebpage ArticlePage = FromArticle
    { title :: String,
      description :: String,
      modifiedDates :: [String],
      tags :: [String],
      fileRequests :: [String]
    }
    deriving (Show, Eq, Generic)
  data ToWebpage ArticlePage = ToArticle
    { fileContents :: [String]
    }
    deriving (Show, Eq, Generic)

instance Binary (FromWebpage ArticlePage)

instance HasTitle (FromWebpage ArticlePage)

instance HasDescription (FromWebpage ArticlePage)

instance HasModifiedDates (FromWebpage ArticlePage)

instance HasTags (FromWebpage ArticlePage)

instance HasFileContentsRequest (FromWebpage ArticlePage)

instance HasFileContentsResponse (ToWebpage ArticlePage)

instance Default (FromWebpage ArticlePage) where
  def =
    FromArticle
      { title = mempty,
        description = mempty,
        modifiedDates = mempty,
        tags = mempty,
        fileRequests = mempty
      }

instance Default (ToWebpage ArticlePage) where
  def = ToArticle {fileContents = []}

instance WebpageHakyllDataExchangeProtocol IndexPage where
  data FromWebpage IndexPage = FromIndex
    { title :: String,
      description :: String,
      modifiedDates :: [String],
      externals :: [Link],
      articles :: [Link],
      fileRequests :: [String]
    }
    deriving (Show, Eq, Generic)
  data ToWebpage IndexPage = ToIndex
    { fileContents :: [String]
    }
    deriving (Show, Eq, Generic)

instance HasTitle (FromWebpage IndexPage)

instance HasDescription (FromWebpage IndexPage)

instance HasModifiedDates (FromWebpage IndexPage)

instance HasFileContentsRequest (FromWebpage IndexPage)

instance HasFileContentsResponse (ToWebpage IndexPage)

instance Default (FromWebpage IndexPage) where
  def =
    FromIndex
      { title = mempty,
        description = mempty,
        modifiedDates = mempty,
        externals = mempty,
        articles = mempty,
        fileRequests = mempty
      }

instance Default (ToWebpage IndexPage) where
  def = ToIndex {fileContents = []}

data WebpageEnvInternal = WebpageEnvInternal
  { headerLevel :: Int,
    annotationIndex :: Int
  }
  deriving (Eq, Show, Generic)

data WebpageEnv (pageType :: WebpageType) = WebpageEnv
  { internal :: WebpageEnvInternal,
    toWebpage :: ToWebpage pageType
  }
  deriving (Generic)

instance Default (WebpageEnv ArticlePage) where
  def = mkDefaultWebpageEnv def

instance Default (WebpageEnv IndexPage) where
  def = mkDefaultWebpageEnv def

mkDefaultWebpageEnv :: ToWebpage pageType -> WebpageEnv pageType
mkDefaultWebpageEnv toWebpage =
  WebpageEnv
    { internal =
        WebpageEnvInternal
          { headerLevel = 0,
            annotationIndex = 0
          },
      toWebpage = toWebpage
    }

https :: (IsString s, Semigroup s) => s -> s
https = (<>) "https://"

h_ ::
  (MonadReader env m, HasHeaderLevel env, Term [Attribute] (m a -> m a), Term (m a) (m a)) =>
  (m a -> m a -> m a)
h_ headerText body =
  let hOfLevel :: forall m a. Term [Attribute] (m a -> m a) => [[Attribute] -> m a -> m a]
      hOfLevel =
        [h1_, h2_, h3_, h4_, h5_, h6_]
   in local (over headerLevelL (+ 1)) $ do
        index <- min 5 . view headerLevelL <$> ask
        L.genericIndex hOfLevel index [] headerText
        body

header ::
  (MonadReader env m, HasHeaderLevel env, Term [Attribute] (m a -> m a), Term (m a) (m a)) =>
  (m a -> m a -> m a)
header headerText = header_ [] . h_ headerText

section ::
  (MonadReader env m, HasHeaderLevel env, Term [Attribute] (m a -> m a), Term (m a) (m a)) =>
  (m a -> m a -> m a)
section headerText body = section_ [] $ h_ headerText body

paragraph ::
  (MonadReader env m, HasHeaderLevel env, Term [Attribute] (m a -> m a), Term (m a) (m a)) =>
  (m a -> m a)
paragraph = p_ []

deriving instance Generic FeedConfiguration

instance Binary FeedConfiguration

defaultFeedConfig :: FeedConfiguration
defaultFeedConfig =
  FeedConfiguration
    { feedTitle = "linguiniのブログ",
      feedDescription = "linguiniがなんか書く",
      feedAuthorName = "linguini",
      feedAuthorEmail = "9647142@gmail.com",
      feedRoot = "https://1inguini.github.io"
    }

doctypehtml_ :: WebpageBody pageType () -> WebpageBody pageType ()
doctypehtml_ = WebpageBody . Lucid.doctypehtml_ . runWebpageBody

link_, meta_ :: [Attribute] -> WebpageBody pageType ()
link_ = WebpageBody . Lucid.link_
meta_ = WebpageBody . Lucid.meta_

prefix_, property_ :: Text -> Attribute
prefix_ = Lucid.makeAttribute "prefix"
property_ = Lucid.makeAttribute "property"

-- mkBlogPost :: Monad m => FeedConfiguration -> HtmlT m () -> BlogPost m
articleCommon :: Webpage pageType -> Webpage pageType
articleCommon webpage =
  set
    (webpageBodyL :: Lens' (Webpage pageType) (WebpageBody pageType ()))
    (article_ $ h_ (toWebpageBody (view titleL webpage)) $ view webpageBodyL webpage)
    webpage

webpageCommon :: Webpage pageType -> Webpage pageType
webpageCommon webpage =
  let og property content = meta_ [property_ ("og:" <> property), content_ content]
   in webpage
        { webpageBody =
            doctypehtml_ $ do
              head_ [prefix_ "og:https://ogp.me/ns#"] $ do
                meta_ [charset_ "utf-8"]
                title_ $ toWebpageBodyRaw $ view titleL webpage
                og "title" $ T.pack $ view titleL webpage
                meta_ [name_ "linguini", content_ "blog"]
                meta_ [name_ "generator", content_ "Hakyll"]
                meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
                link_ [rel_ "icon", href_ "https://avatars0.githubusercontent.com/u/42938754"]
                -- link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/bulma@0.9.1/css/bulma.min.css"]
                link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/@exampledev/new.css/new.min.css"]
              body_ $ do
                header_ $ h1_ "linguiniの✨ブログ✨" :: WebpageBody pageType ()
                main_ $ view webpageBodyL webpage
                footer_ $ "Copyright: © 2020 linguini. Site proudly generated by " <> hyperlink "http://jaspervdj.be/hakyll" "Hakyll" <> ". Visit the site repository from " <> hyperlink "https://github.com/1inguini/1inguini.github.io" "here" <> "."
        }

type Link = (FilePath, String)

hyperlinkInternal :: Term [Attribute] result => Text -> result
hyperlinkInternal l = a_ [href_ l]

hyperlink :: Term [Attribute] result => Text -> result
hyperlink l = a_ (href_ l : newTabAttr)

newTabAttr :: [Attribute]
newTabAttr = [target_ "_blank", rel_ "noreferrer noopener"]

hyperlinkEcho :: Text -> WebpageBody pageType ()
hyperlinkEcho l = hyperlink l $ toWebpageBodyRaw l

hyperlinkGitHub :: Text -> WebpageBody pageType ()
hyperlinkGitHub repo =
  a_ (href_ (https "github.com/" <> repo) : newTabAttr) $ toWebpageBodyRaw repo

hyperlinkList :: Foldable t => [Attribute] -> t Link -> WebpageBody pageType ()
hyperlinkList attrs ls =
  ul_ $
    mapM_
      ( \(path, desc) ->
          li_ $ a_ (href_ (T.pack path) : attrs) (toWebpageBodyRaw desc)
      )
      ls

code ::
  (MonadReader env m, HasHeaderLevel env, Term [Attribute] (m a -> m a), Term (m a) (m a)) =>
  (m a -> m a)
code = code_
