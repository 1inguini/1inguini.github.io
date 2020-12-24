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
    HasWebpageCommonData (..),
    WebpageHakyllDataExchangeProtocol (..),
    ArticleProtocol (..),
    IndexProtocol (..),
    Webpage (..),
    WebpageBody (..),
    WebpageCommonData (..),
    WebpageEnv (..),
    Link (..),
    articleCommon,
    code,
    defaultFeedConfig,
    header,
    https,
    hyperlink,
    hyperlinkEcho,
    hyperlinkGitHub,
    hyperlinkInternal,
    hyperlinkList,
    mkDefaultWebpageEnv,
    paragraph,
    renderWebpageBody,
    section,
    siteName,
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
import Lucid hiding (br_, doctypehtml_, input_, link_, meta_)
import qualified Lucid (br_, doctypehtml_, input_, link_, meta_)
import qualified Lucid.Base as Lucid (makeAttribute)
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.List as L
import RIO.State
import qualified RIO.Text as T
import RIO.Time
import Share

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
  fileContentsL :: Lens' hasResponse [String]
  default fileContentsL :: HasField' "fileContents" hasResponse [String] => Lens' hasResponse [String]
  fileContentsL = field' @"fileContents"

class HasTags a where
  tagsL :: Lens' a [String]
  default tagsL :: HasField' "tags" a [String] => Lens' a [String]
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
    HasFileContentsResponse (protocol False)
  ) =>
  WebpageHakyllDataExchangeProtocol (protocol :: Bool -> *)

data WebpageCommonData = WebpageCommonData
  { title :: String,
    description :: String,
    modifiedDates :: [String] -- older first iso8601
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
  { title :: String,
    description :: String,
    modifiedDates :: [String],
    tags :: [String],
    fileRequests :: [String]
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

newtype instance ArticleProtocol False = ToArticle
  { fileContents :: [String]
  }
  deriving (Show, Eq, Generic)

instance Default (ArticleProtocol False) where
  def = ToArticle mempty

instance HasFileContentsResponse (ArticleProtocol False)

-- end of WebpageHakyllDataExchangeProtocol for Article

-- WebpageHakyllDataExchangeProtocol for Index
instance WebpageHakyllDataExchangeProtocol IndexProtocol

data family IndexProtocol (isFromWebpage :: Bool)

data instance IndexProtocol True = FromIndex
  { title :: String,
    description :: String,
    modifiedDates :: [String],
    fileRequests :: [String]
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
  { externals :: [Link],
    articles :: [Link],
    fileContents :: [String]
  }
  deriving (Show, Eq, Generic)

instance Default (IndexProtocol False) where
  def = ToIndex mempty mempty mempty

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

siteName = "linguiniのブログ"

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

-- header ::
--   ( MonadReader env m,
--     HasHeaderLevel env,
--     Term [Attribute] (m () -> m ()),
--     Term (m ()) (m ())
--   ) =>
--   (m () -> m ())
header ::
  WebpageHakyllDataExchangeProtocol protocol =>
  (WebpageBody protocol () -> WebpageBody protocol ())
header headerText = header_ [] $ h_ headerText (pure ())

-- section ::
--   (MonadReader env m, HasHeaderLevel env, Term [Attribute] (m a -> m a), Term (m a) (m a)) =>
--   (m a -> m a -> m a)
section ::
  WebpageHakyllDataExchangeProtocol protocol =>
  (WebpageBody protocol () -> WebpageBody protocol () -> WebpageBody protocol ())
section headerText body = section_ [] $ h_ headerText body

-- paragraph ::
--   (MonadReader env m, HasHeaderLevel env, Term [Attribute] (m a -> m a), Term (m a) (m a)) =>
--   (m a -> m a)
paragraph ::
  WebpageHakyllDataExchangeProtocol protocol =>
  (WebpageBody protocol () -> WebpageBody protocol ())
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

doctypehtml_ :: WebpageBody protocol () -> WebpageBody protocol ()
doctypehtml_ = WebpageBody . Lucid.doctypehtml_ . runWebpageBody

link_, meta_, input_, br_ :: [Attribute] -> WebpageBody protocol ()
link_ = WebpageBody . Lucid.link_
meta_ = WebpageBody . Lucid.meta_
input_ = WebpageBody . Lucid.input_
br_ = WebpageBody . Lucid.input_

prefix_, property_ :: Text -> Attribute
prefix_ = Lucid.makeAttribute "prefix"
property_ = Lucid.makeAttribute "property"

-- mkBlogPost :: Monad m => FeedConfiguration -> HtmlT m () -> BlogPost m
articleCommon ::
  WebpageHakyllDataExchangeProtocol protocol =>
  Webpage protocol ->
  Webpage protocol
articleCommon webpage =
  set
    (webpageBodyL :: Lens' (Webpage protocol) (WebpageBody protocol ()))
    (article_ $ h_ (toWebpageBody (view titleL webpage)) $ view webpageBodyL webpage)
    webpage

webpageCommon ::
  WebpageHakyllDataExchangeProtocol protocol =>
  Webpage protocol ->
  Webpage protocol
webpageCommon webpage =
  let og property content = meta_ [property_ ("og:" <> property), content_ content]
   in webpage
        { webpageBody =
            doctypehtml_ $ do
              head_ [prefix_ "og:https://ogp.me/ns#"] $ do
                meta_ [charset_ "utf-8"]
                title_ $ toWebpageBodyRaw $ view titleL webpage
                og "site_name" siteName
                og "image" "https://avatars0.githubusercontent.com/u/42938754?s=400&v=4"
                og "title" $ T.pack $ view titleL webpage
                og "description" $ T.pack $ view descriptionL webpage
                meta_ [name_ "twitter:card", content_ "summary"]
                meta_ [name_ "linguini", content_ "blog"]
                meta_ [name_ "generator", content_ "Hakyll"]
                meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
                link_ [rel_ "icon", href_ "https://avatars0.githubusercontent.com/u/42938754"]
                -- link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/bulma@0.9.1/css/bulma.min.css"]
                link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/@exampledev/new.css/new.min.css"]
              body_ $ do
                header "linguiniの✨ブログ✨"
                main_ $ view webpageBodyL webpage
                section "コメント欄" $
                  form_ [method_ "POST", action_ "https://staticman-1inguini.herokuapp.com/v2/entry/1inguini/1inguini.github.io/master/comments"] $ do
                    input_ [name_ "options[title]", type_ "hidden", value_ $ T.pack $ view titleL webpage]
                    label_ $ "ハンドルネーム" <> input_ [name_ "fields[name]", type_ "text"]
                    label_ $ "E-Mail(Optional)" <> input_ [name_ "fields[email]", type_ "email"]
                    textarea_ [name_ "fields[message]", rows_ "10", cols_ "100", placeholder_ "マサカリを投げる"] (pure ()) :: WebpageBody protocol ()
                    button_ [type_ "submit"] "送信" :: WebpageBody protocol ()
                footer_ $ "Copyright: © 2020 linguini. Site proudly generated by " <> hyperlink "http://jaspervdj.be/hakyll" "Hakyll" <> ". Visit the site repository from " <> hyperlink "https://github.com/1inguini/1inguini.github.io" "here" <> "."
        }

type Link = (FilePath, String)

-- hyperlinkInternal :: Term [Attribute] result => Text -> result
hyperlinkInternal ::
  WebpageHakyllDataExchangeProtocol protocol =>
  (Text -> WebpageBody protocol () -> WebpageBody protocol ())
hyperlinkInternal l = a_ [href_ l]

-- hyperlink :: Term [Attribute] result => Text -> result
hyperlink ::
  WebpageHakyllDataExchangeProtocol protocol =>
  (Text -> WebpageBody protocol () -> WebpageBody protocol ())
hyperlink l = a_ (href_ l : newTabAttr)

newTabAttr :: [Attribute]
newTabAttr = [target_ "_blank", rel_ "noreferrer noopener"]

hyperlinkEcho ::
  WebpageHakyllDataExchangeProtocol protocol =>
  (Text -> WebpageBody protocol ())
hyperlinkEcho l = hyperlink l $ toWebpageBodyRaw l

hyperlinkGitHub ::
  WebpageHakyllDataExchangeProtocol protocol =>
  (Text -> WebpageBody protocol ())
hyperlinkGitHub repo =
  a_ (href_ (https "github.com/" <> repo) : newTabAttr) $ toWebpageBodyRaw repo

hyperlinkList ::
  (Foldable t, WebpageHakyllDataExchangeProtocol protocol) =>
  (Bool -> t Link -> WebpageBody protocol ())
hyperlinkList isExternal ls =
  ul_ $
    mapM_
      ( \(path, desc) ->
          li_ $ a_ (href_ (T.pack path) : (if isExternal then newTabAttr else [])) (toWebpageBodyRaw desc)
      )
      ls

-- code ::
--   (MonadReader env m, HasHeaderLevel env, Term [Attribute] (m a -> m a), Term (m a) (m a)) =>
--   (m a -> m a)
code ::
  WebpageHakyllDataExchangeProtocol protocol =>
  (WebpageBody protocol () -> WebpageBody protocol ())
code = code_
