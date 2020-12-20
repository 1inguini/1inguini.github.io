{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | basic templates
module Template
  ( module RIO,
    module Hakyll,
    module Lucid,
    module Data.Functor.Identity,
    HasAnnotationIndex (..),
    HasDefault (..),
    HasDescription (..),
    HasFileContentsRequest (..),
    HasFileContentsResponse (..),
    -- HasFileContentsRequestResponse (..),
    HasHeaderLevel (..),
    HasModifiedDates (..),
    HasTitle (..),
    HasWebpageEnvFromHakyll (..),
    ArticleData (..),
    FromHakyll (..),
    IndexData (..),
    Webpage (..),
    WebpageBody (..),
    WebpageEnv (..),
    Link (..),
    aHref,
    aHrefEcho,
    aHrefGitHub,
    aHrefInternal,
    aHrefList,
    articleCommon,
    defaultFeedConfig,
    defaultWebpageEnv,
    h_,
    https,
    link_,
    meta_,
    newTabAttr,
    renderWebpageBody,
    toWebpageBody,
    toWebpageBodyRaw,
    webpageCommon,
  )
where

-- Contex StringはStringのレコード(Map)みたいなもの!(暴論)

import Data.Binary
import Data.Functor.Identity
import Data.Kind (Constraint)
import Data.String (IsString)
import Data.Typeable
import GHC.Generics (Generic)
import Hakyll (FeedConfiguration (..))
import Lucid hiding (doctypehtml_, link_, meta_, nk_)
import qualified Lucid (doctypehtml_, link_, meta_)
import qualified Lucid.Base as Lucid (makeAttribute)
import RIO hiding (for_)
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.List as L
import RIO.State
import qualified RIO.Text as T
import RIO.Time

class HasDefault a where
  theDefault :: a

data WebpageData = WebpageData
  { title :: String,
    description :: String,
    modifiedDates :: [String] -- older first iso8601
  }
  deriving (Show, Generic)

instance Binary WebpageData

class HasTitle a where
  titleL :: Lens' a String

class HasDescription a where
  descriptionL :: Lens' a String

class HasModifiedDates a where
  modifiedDatesL :: Lens' a [String]

instance HasTitle WebpageData where
  titleL = lens description $ \s a -> s {title = a}

instance HasDescription WebpageData where
  descriptionL = lens description $ \s a -> s {description = a}

instance HasModifiedDates WebpageData where
  modifiedDatesL = lens modifiedDates $ \s a -> s {modifiedDates = a}

-- -- data ExampleFileContentRequests a = Example {codeSnippet0, codeSnippet1 :: a} deriving (Functor)といった型を定義してExampleFileContentRequests FilePathで内容がほしいファイルを指定、HakyllがExampleFileContentRequests String(このStringはファイルの中身)といった形で返してくれることを期待する
-- -- Functional DependenciesがないとfileContentRequestsL(もしくはfileContentsL)だけ見たときにHasFileContentsRequestResponse AProtocol ARequest AResponseの可能性だけでなくHasFileContentsRequestResponse AProtocol ARequest BOtherResponseSuchAsIntの可能性も出てきてしまうのでエラーが出る、これブログに書こう
-- class
--   Functor protocol =>
--   HasFileContentsRequestResponse protocol hasRequest hasResponse
--     | protocol -> hasRequest hasResponse
--   where
--   fileContentsRequestL :: Lens' hasRequests (protocol FilePath)
--   fileContentsL :: Lens' hasResposes (protocol String)

class HasFileContentsRequest hasRequest where
  hasRequestL :: Lens' hasRequest (Vector FilePath)

class HasFileContentsResponse hasResponse where
  hasResponseL :: Lens' hasResponse (Vector String)

data Webpage pageTypeSpecific = Webpage
  { webpageData :: WebpageData,
    fileContentsRequest :: Vector FilePath,
    body :: WebpageBody pageTypeSpecific ()
  }

instance HasFileContentsRequest (Webpage pageTypeSpecific) where
  hasRequestL = lens fileContentsRequest $
    \s a -> s {fileContentsRequest = a}

class HasWebpageData a where
  webpageDataL :: Lens' a WebpageData

instance HasWebpageData (Webpage pageTypeSpecific) where
  webpageDataL = lens webpageData $ \s a -> s {webpageData = a}

class HasWebpageBody pageTypeSpecific a where
  webpageBodyL :: Lens' a (WebpageBody pageTypeSpecific ())

instance HasWebpageBody pageTypeSpecific (Webpage pageTypeSpecific) where
  webpageBodyL = lens body $ \s a -> s {body = a}

instance HasTitle (Webpage pageTypeSpecific) where
  titleL = lens (webpageData >>> view titleL) $
    \s a -> s {webpageData = set titleL a (webpageData s)}

instance HasDescription (Webpage pageTypeSpecific) where
  descriptionL = lens (webpageData >>> view descriptionL) $
    \s a -> s {webpageData = set descriptionL a (webpageData s)}

instance HasModifiedDates (Webpage pageTypeSpecific) where
  modifiedDatesL = lens (webpageData >>> view modifiedDatesL) $
    \s a -> s {webpageData = set modifiedDatesL a (webpageData s)}

newtype WebpageBody pageTypeSpecific a = WebpageBody {runWebpageBody :: HtmlT (Reader (WebpageEnv pageTypeSpecific)) a}
  deriving (Functor, Applicative, Monad, MonadReader (WebpageEnv pageTypeSpecific))

deriving instance Semigroup (WebpageBody pageTypeSpecific ())

instance HasDefault pageTypeSpecific => Show (WebpageBody pageTypeSpecific ()) where
  show = show . renderWebpageBody theDefault

instance IsString (WebpageBody pageTypeSpecific ()) where
  fromString = WebpageBody . toHtmlRaw

instance (f ~ WebpageBody pageTypeSpecific a) => Term [Attribute] (f -> WebpageBody pageTypeSpecific a) where
  termWith name f attr = WebpageBody . termWith name f attr . runWebpageBody

instance Term (WebpageBody pageTypeSpecific a) (WebpageBody pageTypeSpecific a) where
  termWith name f = WebpageBody . termWith name f . runWebpageBody

toWebpageBody :: ToHtml a => a -> WebpageBody pageTypeSpecific ()
toWebpageBody = WebpageBody . toHtml

toWebpageBodyRaw :: ToHtml a => a -> WebpageBody pageTypeSpecific ()
toWebpageBodyRaw = WebpageBody . toHtmlRaw

renderWebpageBody :: WebpageEnv pageTypeSpecific -> WebpageBody pageTypeSpecific () -> BL.ByteString
renderWebpageBody env body =
  runWebpageBody body
    & renderBST
    & (`runReader` env)

data FromHakyll pageTypeSpecific = FromHakyll
  { fileContents :: Vector String,
    pageTypeSpecific :: pageTypeSpecific
  }

instance HasDefault pageTypeSpecific => HasDefault (FromHakyll pageTypeSpecific) where
  theDefault =
    FromHakyll
      { fileContents = mempty,
        pageTypeSpecific = theDefault :: pageTypeSpecific
      }

data IndexData = IndexData
  { externals :: [Link],
    articles :: [Link]
  }
  deriving (Show, Eq)

instance HasDefault IndexData where
  theDefault = IndexData {externals = [], articles = []}

data ArticleData = ArticleData
  deriving (Eq, Show, Generic)

-- instance
--   Functor protocol =>
--   HasFileContentsRequest (Webpage IndexData) protocol
--   where
--   hasRequestL = lens (\Webpage {fileContentsRequest = x} -> x) $
--     \s a -> s {fileContentsRequest = a}

data WebpageEnvInternal = WebpageEnvInternal
  { headerLevel :: Int,
    annotationIndex :: Int
  }
  deriving (Eq, Show, Generic)

class HasHeaderLevel env where
  headerLevelL :: Lens' env Int

class HasAnnotationIndex env where
  annotationIndexL :: Lens' env Int

instance HasHeaderLevel WebpageEnvInternal where
  headerLevelL = lens headerLevel $
    \s a -> s {headerLevel = a}

instance HasAnnotationIndex WebpageEnvInternal where
  annotationIndexL = lens annotationIndex $
    \s a -> s {annotationIndex = a}

data WebpageEnv pageTypeSpecific = WebpageEnv
  { internal :: WebpageEnvInternal,
    fromHakyll :: FromHakyll pageTypeSpecific
  }
  deriving (Generic)

class HasWebpageEnvFromHakyll pageTypeSpecific env where
  webpageEnvFromHakyllL :: Lens' env pageTypeSpecific

instance HasWebpageEnvFromHakyll (FromHakyll pageTypeSpecific) (WebpageEnv pageTypeSpecific) where
  webpageEnvFromHakyllL = lens fromHakyll $
    \s a -> s {fromHakyll = a}

instance HasDefault pageTypeSpecific => HasDefault (WebpageEnv pageTypeSpecific) where
  theDefault = defaultWebpageEnv theDefault

defaultWebpageEnv :: FromHakyll pageTypeSpecific -> WebpageEnv pageTypeSpecific
defaultWebpageEnv fromHakyll =
  WebpageEnv
    { internal =
        WebpageEnvInternal
          { headerLevel = 0,
            annotationIndex = 0
          },
      fromHakyll = fromHakyll
    }

instance HasHeaderLevel (WebpageEnv pageTypeSpecific) where
  headerLevelL = lens (internal >>> headerLevel) $
    \s a -> s {internal = set headerLevelL a $ internal s}

instance HasAnnotationIndex (WebpageEnv pageTypeSpecific) where
  annotationIndexL = lens (internal >>> annotationIndex) $
    \s a -> s {internal = set annotationIndexL a $ internal s}

https :: (IsString s, Semigroup s) => s -> s
https = (<>) "https://"

h_ :: (MonadReader env m, HasHeaderLevel env, Term arg (m a)) => arg -> m a
h_ arg =
  let hOfLevel = [h1_, h2_, h3_, h4_, h5_, h6_]
   in local (over headerLevelL (+ 1)) $ do
        index <- view headerLevelL
        L.genericIndex hOfLevel index arg

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

doctypehtml_ :: WebpageBody pageTypeSpecific () -> WebpageBody pageTypeSpecific ()
doctypehtml_ = WebpageBody . Lucid.doctypehtml_ . runWebpageBody

link_, meta_ :: [Attribute] -> WebpageBody pageTypeSpecific ()
link_ = WebpageBody . Lucid.link_
meta_ = WebpageBody . Lucid.meta_

prefix_, property_ :: Text -> Attribute
prefix_ = Lucid.makeAttribute "prefix"
property_ = Lucid.makeAttribute "property"

-- mkBlogPost :: Monad m => FeedConfiguration -> HtmlT m () -> BlogPost m
articleCommon :: Text -> WebpageBody pageTypeSpecific () -> WebpageBody pageTypeSpecific ()
articleCommon title = article_ . (h_ (toWebpageBodyRaw title) <>)

webpageCommon :: Webpage pageTypeSpecific -> Webpage pageTypeSpecific
webpageCommon webpage =
  let og property content = meta_ [property_ ("og:" <> property), content_ content]
   in webpage
        { body =
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
                header_ $ h1_ "linguiniの✨ブログ✨" :: WebpageBody pageTypeSpecific ()
                main_ $ body webpage
                footer_ $ "Copyright: © 2020 linguini. Site proudly generated by " <> aHref "http://jaspervdj.be/hakyll" "Hakyll" <> ". Visit the site repository from " <> aHref "https://github.com/1inguini/1inguini.github.io" "here" <> "."
        }

type Link = (FilePath, String)

aHrefInternal :: Term [Attribute] result => Text -> result
aHrefInternal l = a_ [href_ l]

aHref :: Term [Attribute] result => Text -> result
aHref l = a_ (href_ l : newTabAttr)

newTabAttr :: [Attribute]
newTabAttr = [target_ "_blank", rel_ "noreferrer noopener"]

aHrefEcho :: Text -> WebpageBody pageTypeSpecific ()
aHrefEcho l = aHref l $ toWebpageBodyRaw l

aHrefGitHub :: Text -> WebpageBody pageTypeSpecific ()
aHrefGitHub repo =
  a_ (href_ (https "github.com/" <> repo) : newTabAttr) $ toWebpageBodyRaw repo

aHrefList :: Foldable t => [Attribute] -> t Link -> WebpageBody pageTypeSpecific ()
aHrefList attrs ls =
  ul_ $
    mapM_
      ( \(path, desc) ->
          li_ $ a_ (href_ (T.pack path) : attrs) (toWebpageBodyRaw desc)
      )
      ls
