-- | my custom Prelude (basicly re-exporting RIO with some things hided)
module Share
  ( module Data.Default.Class,
    module Data.Generics.Product,
    module GHC.Generics,
    module Optics,
    module RIO,
    module RIO.List,
    HasAnnotationIndex (..),
    HasDescription (..),
    HasFileContentsRequest (..),
    HasFileContentsResponse (..),
    HasHeaderLevel (..),
    HasModifiedDates (..),
    HasPath (..),
    HasTags (..),
    HasTitle (..),
    HasWebpageBody (..),
    HasWebpageCommonData (..),
    WebpageHakyllDataExchangeProtocol (..),
    ArticleProtocol (..),
    IndexProtocol (..),
    Webpage (..),
    WebpageBody (..),
    WebpageCommonData (..),
    WebpageEnv (..),
    Link (..),
    defaultFeedConfig,
    https,
    mkDefaultWebpageEnv,
    renderWebpageBody,
    siteName,
    toWebpageBody,
    toWebpageBodyRaw,
  )
where

import Data.Binary (Binary)
import Data.Default.Class
import Data.Generics.Product
import Data.Typeable
import GHC.Base (Symbol)
import GHC.Generics (Generic)
import Hakyll (FeedConfiguration (..))
import Lucid (Attribute, HtmlT, Term (..), ToHtml (..), renderBST)
import Optics
import RIO hiding
  ( Lens (..),
    Lens' (..),
    for_,
    lens,
    over,
    preview,
    set,
    sets,
    to,
    view,
    (%~),
    (.~),
    (^.),
    (^..),
    (^?),
  )
import qualified RIO.ByteString.Lazy as BL
import RIO.List hiding (uncons)

class HasPath a where
  pathL :: Lens' a FilePath
  default pathL :: HasField' "path" a FilePath => Lens' a FilePath
  pathL = field' @"path"

class HasTitle a where
  titleL :: Lens' a Text
  default titleL :: HasField' "title" a Text => Lens' a Text
  titleL = field' @"title"

class HasDescription a where
  descriptionL :: Lens' a Text
  default descriptionL :: HasField' "description" a Text => Lens' a Text
  descriptionL = field' @"description"

class HasModifiedDates a where
  modifiedDatesL :: Lens' a [Text]
  default modifiedDatesL :: HasField' "modifiedDates" a [Text] => Lens' a [Text]
  modifiedDatesL = field' @"modifiedDates"

class HasWebpageBody (protocol :: Bool -> *) a where
  webpageBodyL :: Lens' a (WebpageBody protocol ())
  default webpageBodyL ::
    HasField' "webpageBody" a (WebpageBody protocol ()) =>
    Lens' a (WebpageBody protocol ())
  webpageBodyL = field' @"webpageBody"

class
  ( HasTitle a,
    HasDescription a,
    HasModifiedDates a,
    HasFileContentsRequest a
  ) =>
  HasWebpageCommonData a
  where
  webpageCommonDataL :: Lens' a WebpageCommonData
  default webpageCommonDataL ::
    (Generic a, Subtype WebpageCommonData a) =>
    Lens' a WebpageCommonData
  webpageCommonDataL = super @WebpageCommonData

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
  fileContentsL :: Lens' hasResponse [BL.ByteString]
  default fileContentsL :: HasField' "fileContents" hasResponse [BL.ByteString] => Lens' hasResponse [BL.ByteString]
  fileContentsL = field' @"fileContents"

class HasTags a where
  tagsL :: Lens' a [Text]
  default tagsL :: HasField' "tags" a [Text] => Lens' a [Text]
  tagsL = field' @"tags"

data Webpage (protocol :: Bool -> *) = Webpage
  { webpageData :: protocol True,
    webpageBody :: WebpageBody protocol ()
  }
  deriving (Generic)

instance Default (Webpage ArticleProtocol) where
  def = Webpage def def

instance Default (Webpage IndexProtocol) where
  def = Webpage def def

instance HasTags (Webpage ArticleProtocol) where
  tagsL = field' @"webpageData" % tagsL

instance
  WebpageHakyllDataExchangeProtocol protocol =>
  HasTitle (Webpage protocol)
  where
  titleL = field' @"webpageData" % titleL

instance
  WebpageHakyllDataExchangeProtocol protocol =>
  HasModifiedDates (Webpage protocol)
  where
  modifiedDatesL = field' @"webpageData" % modifiedDatesL

instance
  WebpageHakyllDataExchangeProtocol protocol =>
  HasDescription (Webpage protocol)
  where
  descriptionL = field' @"webpageData" % descriptionL

instance
  WebpageHakyllDataExchangeProtocol protocol =>
  HasFileContentsRequest (Webpage protocol)
  where
  fileRequestsL = field' @"webpageData" % fileRequestsL

instance
  WebpageHakyllDataExchangeProtocol protocol =>
  HasWebpageCommonData (Webpage protocol)
  where
  webpageCommonDataL = field' @"webpageData" % webpageCommonDataL

instance HasWebpageBody protocol (Webpage protocol)

-- -- data ExampleFileContentRequests a = Example {codeSnippet0, codeSnippet1 :: a} deriving (Functor)といった型を定義してExampleFileContentRequests FilePathで内容がほしいファイルを指定、HakyllがExampleFileContentRequests String(このStringはファイルの中身)といった形で返してくれることを期待する
-- -- Functional DependenciesがないとfileContentRequestsL(もしくはfileContentsL)だけ見たときにHasFileContentsRequestResponse AProtocol ARequest AResponseの可能性だけでなくHasFileContentsRequestResponse AProtocol ARequest BOtherResponseSuchAsIntの可能性も出てきてしまうのでエラーが出る、これブログに書こう
-- class
--   Functor protocol =>
--   HasFileContentsRequestResponse protocol hasRequest hasResponse
--     | protocol -> hasRequest hasResponse
--   where
--   fileContentsRequestL :: Lens' hasRequests (protocol FilePath)
--   fileContentsL :: Lens' hasResposes (protocol String)

newtype WebpageBody (protocol :: Bool -> *) a = WebpageBody {runWebpageBody :: HtmlT (Reader (WebpageEnv protocol)) a}
  deriving (Generic, Functor, Applicative, Monad, MonadReader (WebpageEnv protocol))

deriving instance Semigroup (WebpageBody protocol ())

instance Default (WebpageBody protocol ()) where
  def = WebpageBody mempty

instance Default (WebpageEnv protocol) => Show (WebpageBody protocol ()) where
  show = show . renderWebpageBody def

instance IsString (WebpageBody protocol ()) where
  fromString = WebpageBody . toHtmlRaw

instance (f ~ WebpageBody protocol a) => Term [Attribute] (f -> WebpageBody protocol a) where
  termWith name f attr = WebpageBody . termWith name f attr . runWebpageBody

instance Term (WebpageBody protocol a) (WebpageBody protocol a) where
  termWith name f = WebpageBody . termWith name f . runWebpageBody

toWebpageBody :: ToHtml a => a -> WebpageBody protocol ()
toWebpageBody = WebpageBody . toHtml

toWebpageBodyRaw :: ToHtml a => a -> WebpageBody protocol ()
toWebpageBodyRaw = WebpageBody . toHtmlRaw

renderWebpageBody :: WebpageEnv protocol -> WebpageBody protocol () -> BL.ByteString
renderWebpageBody env body =
  runWebpageBody body
    & renderBST
    & (`runReader` env)

-- class WebpageHakyllDataExchangeProtocol (protocol :: Webprotocol) where
--   data FromWebpage protocol :: *
--   data ToWebpage protocol :: *

class
  ( HasWebpageCommonData (protocol True),
    HasPath (protocol False),
    HasFileContentsResponse (protocol False)
  ) =>
  WebpageHakyllDataExchangeProtocol (protocol :: Bool -> *)

data WebpageCommonData = WebpageCommonData
  { title :: Text,
    description :: Text,
    modifiedDates :: [Text] -- older first iso8601
  }
  deriving (Show, Generic)

instance Binary WebpageCommonData

instance HasTitle WebpageCommonData

instance HasDescription WebpageCommonData

instance HasModifiedDates WebpageCommonData

-- WebpageHakyllDataExchangeProtocol for Article
instance WebpageHakyllDataExchangeProtocol ArticleProtocol

data family ArticleProtocol (isFromWebpage :: Bool)

data instance ArticleProtocol True = FromArticle
  { title :: Text,
    description :: Text,
    modifiedDates :: [Text],
    tags :: [Text],
    fileRequests :: [FilePath]
  }
  deriving (Show, Eq, Generic)

instance HasWebpageCommonData (ArticleProtocol True)

instance HasTitle (ArticleProtocol True)

instance HasDescription (ArticleProtocol True)

instance HasModifiedDates (ArticleProtocol True)

instance HasFileContentsRequest (ArticleProtocol True)

instance HasTags (ArticleProtocol True)

instance Binary (ArticleProtocol True)

instance Default (ArticleProtocol True) where
  def = FromArticle mempty mempty mempty mempty mempty

data instance ArticleProtocol False = ToArticle
  { path :: FilePath,
    fileContents :: [BL.ByteString]
  }
  deriving (Show, Eq, Generic)

instance Default (ArticleProtocol False) where
  def = ToArticle mempty mempty

instance HasPath (ArticleProtocol False)

instance HasFileContentsResponse (ArticleProtocol False)

-- end of WebpageHakyllDataExchangeProtocol for Article

-- WebpageHakyllDataExchangeProtocol for Index
instance WebpageHakyllDataExchangeProtocol IndexProtocol

data family IndexProtocol (isFromWebpage :: Bool)

data instance IndexProtocol True = FromIndex
  { title :: Text,
    description :: Text,
    modifiedDates :: [Text],
    fileRequests :: [FilePath]
  }
  deriving (Show, Eq, Generic)

instance HasWebpageCommonData (IndexProtocol True)

instance HasTitle (IndexProtocol True)

instance HasDescription (IndexProtocol True)

instance HasModifiedDates (IndexProtocol True)

instance HasFileContentsRequest (IndexProtocol True)

instance Binary (IndexProtocol True)

instance Default (IndexProtocol True) where
  def = FromIndex mempty mempty mempty mempty

data instance IndexProtocol False = ToIndex
  { path :: FilePath,
    externals :: [Link],
    articles :: [Link],
    fileContents :: [BL.ByteString]
  }
  deriving (Show, Eq, Generic)

instance Default (IndexProtocol False) where
  def = ToIndex mempty mempty mempty mempty

instance HasPath (IndexProtocol False)

instance HasFileContentsResponse (IndexProtocol False)

-- end of WebpageHakyllDataExchangeProtocol for Index

-- instance WebpageHakyllDataExchangeProtocol IndexPage where
--   data FromWebpage IndexPage = FromIndex
--     { title :: String,
--       description :: String,
--       modifiedDates :: [String],
--       externals :: [Link],
--       articles :: [Link],
--       fileRequests :: [String]
--     }
--     deriving (Show, Eq, Generic)
--   data ToWebpage IndexPage = ToIndex
--     { fileContents :: [String]
--     }
--     deriving (Show, Eq, Generic)

-- instance HasTitle (FromWebpage IndexPage)

-- instance HasDescription (FromWebpage IndexPage)

-- instance HasModifiedDates (FromWebpage IndexPage)

-- instance HasFileContentsRequest (FromWebpage IndexPage)

-- instance HasFileContentsResponse (ToWebpage IndexPage)

-- instance Default (FromWebpage IndexPage) where
--   def =
--     FromIndex
--       { title = mempty,
--         description = mempty,
--         modifiedDates = mempty,
--         externals = mempty,
--         articles = mempty,
--         fileRequests = mempty
--       }

-- instance Default (ToWebpage IndexPage) where
--   def = ToIndex {fileContents = []}

data WebpageEnvInternal = WebpageEnvInternal
  { headerLevel :: Int,
    annotationIndex :: Int
  }
  deriving (Eq, Show, Generic)

instance HasHeaderLevel WebpageEnvInternal

instance HasAnnotationIndex WebpageEnvInternal

data WebpageEnv (protocol :: Bool -> *) = WebpageEnv
  { internal :: WebpageEnvInternal,
    toWebpage :: protocol False
  }
  deriving (Generic)

instance Default (WebpageEnv ArticleProtocol) where
  def = mkDefaultWebpageEnv def

instance HasHeaderLevel (WebpageEnv protocol) where
  headerLevelL = typed @WebpageEnvInternal % headerLevelL

instance HasAnnotationIndex (WebpageEnv protocol) where
  annotationIndexL = typed @WebpageEnvInternal % annotationIndexL

instance
  WebpageHakyllDataExchangeProtocol protocol =>
  HasPath (WebpageEnv protocol)
  where
  pathL = typed @(protocol False) % pathL

instance
  WebpageHakyllDataExchangeProtocol protocol =>
  HasFileContentsResponse (WebpageEnv protocol)
  where
  fileContentsL = typed @(protocol False) % fileContentsL

-- instance Default (WebpageEnv IndexPage) where
--   def = mkDefaultWebpageEnv def

mkDefaultWebpageEnv ::
  protocol False ->
  WebpageEnv protocol
mkDefaultWebpageEnv toWebpage =
  WebpageEnv
    { internal =
        WebpageEnvInternal
          { headerLevel = 0,
            annotationIndex = 0
          },
      toWebpage = toWebpage
    }

siteName :: IsString a => a
siteName = "linguiniのブログ"

siteRoot = https "1inguini.github.io/"

https :: (IsString s, Semigroup s) => s -> s
https = (<>) "https://"

deriving instance Generic FeedConfiguration

instance Binary FeedConfiguration

defaultFeedConfig :: FeedConfiguration
defaultFeedConfig =
  FeedConfiguration
    { feedTitle = "linguiniのブログ",
      feedDescription = "linguiniがなんか書く",
      feedAuthorName = "linguini",
      feedAuthorEmail = "9647142@gmail.com",
      feedRoot = siteRoot
    }

type Link = (FilePath, Text)
