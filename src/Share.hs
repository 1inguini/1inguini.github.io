-- | my custom Prelude (basicly re-exporting RIO with some things hided)
module Share
  ( module Data.Default.Class,
    module Data.Generics.Product,
    module GHC.Generics,
    module Optics,
    module RIO,
    module RIO.List,
    HasAnnotationIndex (..),
    HasComments (..),
    HasDescription (..),
    HasFileContentsRequest (..),
    HasFileContentsResponse (..),
    HasHeaderLevel (..),
    HasModifiedDates (..),
    HasPath (..),
    HasTags (..),
    HasTitle (..),
    HasFromWebpageCommon (..),
    HasToWebpageCommon (..),
    HasWebpageBody (..),
    WebpageHakyllDataExchangeProtocol (..),
    ArticleProtocol (FromArticle, ToArticle),
    CommonProtocol (FromWebpage, ToWebpage),
    IndexProtocol (FromIndex, ToIndex, externals, articles),
    Comment (..),
    Webpage (..),
    WebpageBody (..),
    WebpageEnv (..),
    CommentId (..),
    Comments (..),
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

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import Data.Binary (Binary)
import Data.Default.Class
import Data.Generics.Product
import Data.Typeable
import Foreign.C.Types (CTime (..))
import GHC.Base (Constraint, Symbol)
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
import System.Posix.Types (EpochTime)

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
  HasFromWebpageCommon a
  where
  webpageCommonDataL :: Lens' a (CommonProtocol True)
  default webpageCommonDataL ::
    (Generic a, Subtype (CommonProtocol True) a) =>
    Lens' a (CommonProtocol True)
  webpageCommonDataL = super @(CommonProtocol True)

class
  ( HasPath a,
    HasComments a,
    HasFileContentsResponse a
  ) =>
  HasToWebpageCommon a
  where
  hakyllCommonDataL :: Lens' a (CommonProtocol False)
  default hakyllCommonDataL ::
    (Generic a, Subtype (CommonProtocol False) a) =>
    Lens' a (CommonProtocol False)
  hakyllCommonDataL = super @(CommonProtocol False)

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

class HasComments a where
  commentsL :: Lens' a Comments
  default commentsL :: HasField' "comments" a Comments => Lens' a Comments
  commentsL = field' @"comments"

class HasTags a where
  tagsL :: Lens' a [Text]
  default tagsL :: HasField' "tags" a [Text] => Lens' a [Text]
  tagsL = field' @"tags"

class
  ( HasFromWebpageCommon (protocol True),
    HasToWebpageCommon (protocol False)
  ) =>
  WebpageHakyllDataExchangeProtocol (protocol :: Bool -> *)

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
  tagsL = typed @(ArticleProtocol True) % tagsL

instance
  WebpageHakyllDataExchangeProtocol protocol =>
  HasTitle (Webpage protocol)
  where
  titleL = typed @(protocol True) % titleL

instance
  WebpageHakyllDataExchangeProtocol protocol =>
  HasModifiedDates (Webpage protocol)
  where
  modifiedDatesL = typed @(protocol True) % modifiedDatesL

instance
  WebpageHakyllDataExchangeProtocol protocol =>
  HasDescription (Webpage protocol)
  where
  descriptionL = typed @(protocol True) % descriptionL

instance
  WebpageHakyllDataExchangeProtocol protocol =>
  HasFileContentsRequest (Webpage protocol)
  where
  fileRequestsL = typed @(protocol True) % fileRequestsL

instance
  WebpageHakyllDataExchangeProtocol protocol =>
  HasFromWebpageCommon (Webpage protocol)
  where
  webpageCommonDataL = typed @(protocol True) % webpageCommonDataL

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

newtype WebpageBody (protocol :: Bool -> *) a = WebpageBody
  {runWebpageBody :: HtmlT (Reader (WebpageEnv protocol)) a}
  deriving (Generic, Functor, Applicative, Monad, MonadReader (WebpageEnv protocol))

deriving instance Semigroup (WebpageBody protocol ())

instance Default (WebpageBody protocol ()) where
  def = WebpageBody mempty

instance
  (WebpageHakyllDataExchangeProtocol protocol, Default (WebpageEnv protocol)) =>
  Show (WebpageBody protocol ())
  where
  show = show . renderWebpageBody def

instance IsString (WebpageBody protocol ()) where
  fromString = WebpageBody . toHtmlRaw

instance (f ~ WebpageBody protocol a) => Term [Attribute] (f -> WebpageBody protocol a) where
  termWith name f attr = WebpageBody . termWith name f attr . runWebpageBody

instance Term (WebpageBody protocol a) (WebpageBody protocol a) where
  termWith name f = WebpageBody . termWith name f . runWebpageBody

toWebpageBody ::
  (WebpageHakyllDataExchangeProtocol protocol, ToHtml a) =>
  (a -> WebpageBody protocol ())
toWebpageBody = WebpageBody . toHtml

toWebpageBodyRaw ::
  (WebpageHakyllDataExchangeProtocol protocol, ToHtml a) =>
  (a -> WebpageBody protocol ())
toWebpageBodyRaw = WebpageBody . toHtmlRaw

renderWebpageBody ::
  WebpageHakyllDataExchangeProtocol protocol =>
  (WebpageEnv protocol -> WebpageBody protocol () -> BL.ByteString)
renderWebpageBody env body =
  runWebpageBody body
    & renderBST
    & (`runReader` env)

type Comments = [Comment]

type CommentId = Text

data Comment = Comment
  { _id :: CommentId,
    replying_to :: CommentId,
    name :: Text,
    timestamp :: EpochTime,
    twitter :: Text,
    message :: Text
  }
  deriving (Eq, Show, Generic)

deriving instance Generic CTime

instance Binary CTime

instance Binary Comment

instance Aeson.FromJSON Comment

instance Ord Comment where
  compare c0 c1 = compare (timestamp c0) (timestamp c1)

-- Common WebpageHakyllDataExchangeProtocol
instance WebpageHakyllDataExchangeProtocol CommonProtocol

data family CommonProtocol (isFromWebpage :: Bool)

data instance CommonProtocol True = FromWebpage
  { title :: Text,
    description :: Text,
    modifiedDates :: [Text], -- older first iso8601
    fileRequests :: [FilePath]
  }
  deriving (Show, Generic)

instance Binary (CommonProtocol True)

instance HasFromWebpageCommon (CommonProtocol True)

instance HasTitle (CommonProtocol True)

instance HasDescription (CommonProtocol True)

instance HasModifiedDates (CommonProtocol True)

instance HasFileContentsRequest (CommonProtocol True)

data instance CommonProtocol False = ToWebpage
  { path :: FilePath,
    comments :: Comments,
    fileContents :: [BL.ByteString]
  }
  deriving (Show, Generic)

instance Binary (CommonProtocol False)

instance HasToWebpageCommon (CommonProtocol False)

instance HasPath (CommonProtocol False)

instance HasComments (CommonProtocol False)

instance HasFileContentsResponse (CommonProtocol False)

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

instance HasFromWebpageCommon (ArticleProtocol True)

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
    comments :: Comments,
    fileContents :: [BL.ByteString]
  }
  deriving (Show, Eq, Generic)

instance Default (ArticleProtocol False) where
  def = ToArticle mempty mempty mempty

instance HasToWebpageCommon (ArticleProtocol False)

instance HasPath (ArticleProtocol False)

instance HasComments (ArticleProtocol False)

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

instance HasFromWebpageCommon (IndexProtocol True)

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
    comments :: Comments,
    fileContents :: [BL.ByteString]
  }
  deriving (Show, Eq, Generic)

instance Default (IndexProtocol False) where
  def = ToIndex mempty mempty mempty mempty mempty

instance HasToWebpageCommon (IndexProtocol False)

instance HasPath (IndexProtocol False)

instance HasComments (IndexProtocol False)

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
  HasComments (WebpageEnv protocol)
  where
  commentsL = typed @(protocol False) % commentsL

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
