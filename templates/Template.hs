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
import Hakyll.Web.Html (isExternal)
import Lucid hiding (br_, doctypehtml_, input_, link_, meta_, required_)
import qualified Lucid (br_, doctypehtml_, input_, link_, meta_, required_)
import qualified Lucid.Base as Lucid (makeAttribute)
import qualified RIO.ByteString.Lazy as BL
import RIO.FilePath (dropFileName)
import qualified RIO.List as L
import RIO.State
import qualified RIO.Text as T
import RIO.Time
import Share

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

siteName = "linguiniのブログ"

siteRoot = https "1inguini.github.io/"

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

hyperlinkHeader ::
  WebpageHakyllDataExchangeProtocol protocol =>
  (Text -> WebpageBody protocol () -> WebpageBody protocol ())
hyperlinkHeader url headerText = header $ hyperlink url headerText

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
      feedRoot = siteRoot
    }

doctypehtml_ :: WebpageBody protocol () -> WebpageBody protocol ()
doctypehtml_ = WebpageBody . Lucid.doctypehtml_ . runWebpageBody

link_, meta_, input_, br_ :: [Attribute] -> WebpageBody protocol ()
link_ = WebpageBody . Lucid.link_
meta_ = WebpageBody . Lucid.meta_
input_ = WebpageBody . Lucid.input_
br_ = WebpageBody . Lucid.input_

required_ :: Attribute
required_ = Lucid.required_ mempty

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

enterMoveFocusToNameInForm :: Text -> Attribute
enterMoveFocusToNameInForm name =
  onkeydown_ $
    T.unlines
      [ "if(this.keycode == 13) {",
        "  this.form." <> name <> ".focus();",
        "}"
      ]

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
                og "title" $ view titleL webpage
                og "description" $ view descriptionL webpage
                meta_ [name_ "twitter:card", content_ "summary"]
                meta_ [name_ "linguini", content_ "blog"]
                meta_ [name_ "generator", content_ "Hakyll"]
                meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
                link_ [rel_ "icon", href_ "https://avatars0.githubusercontent.com/u/42938754"]
                -- link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/bulma@0.9.1/css/bulma.min.css"]
                link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/@exampledev/new.css/new.min.css"]
              body_ $ do
                hyperlinkHeader "" "linguiniの✨ブログ✨"
                main_ $ view webpageBodyL webpage
                section "コメント欄" $ do
                  postDir <- T.pack . dropFileName . view pathL <$> ask
                  form_
                    [ onsubmit_ $
                        T.unlines
                          [ "var message = this.value;",
                            "this.value='';",
                            "this.placeholder='コメント送信中...';",
                            "var req = new XMLHttpRequest ();",
                            "",
                            "req.open(",
                            "  \"POST\",",
                            "  'https://staticman-1inguini.herokuapp.com/v2/entry/1inguini/1inguini.github.io/master/comments',",
                            "  true",
                            ");",
                            "",
                            "req.setRequestHeader(",
                            "  'Content-Type',",
                            "  'application/x-www-form-urlencoded;charset=UTF-8'",
                            ");",
                            "",
                            "req.onreadystatechange = function() {",
                            "  if (this.readyState === XMLHttpRequest.DONE && this.status == 200) {",
                            "    this.placeholder='マサカリを投げる';",
                            "  }",
                            "}",
                            "",
                            "var query =",
                            "  'options[title]=' + encodeURIComponent('" <> view titleL webpage <> "')",
                            "  + '&options[path]=' + encodeURIComponent('" <> postDir <> "')",
                            "  + '&fields[name]=' + encodeURIComponent(this.name.value)",
                            "  + '&fields[message]=' + encodeURIComponent(message);",
                            "if (this.twitter.value) {",
                            "  query += '&fields[twitter]=' + encodeURIComponent(this.twitter.value);",
                            "}",
                            "req.send(query);",
                            "",
                            "return false;"
                          ]
                    ]
                    $ do
                      label_ $ "ハンドルネーム" <> input_ [name_ "name", type_ "text", required_, enterMoveFocusToNameInForm "twitter"]
                      label_ $ "twitter垢(任意)" <> input_ [name_ "twitter", type_ "text", placeholder_ "1inguini", pattern_ "^(\\w{1,15})$", enterMoveFocusToNameInForm "message"]
                      textarea_ [name_ "message", rows_ "10", cols_ "100", placeholder_ "マサカリを投げる", required_] (pure ()) :: WebpageBody protocol ()
                      button_ [type_ "submit"] "送信" :: WebpageBody protocol ()
                footer_ $ "Copyright: © 2020 linguini. Site proudly generated by " <> hyperlink "http://jaspervdj.be/hakyll" "Hakyll" <> ". Visit the site repository from " <> hyperlink "https://github.com/1inguini/1inguini.github.io" "here" <> "."
        }

type Link = (FilePath, Text)

-- hyperlink :: Term [Attribute] result => Text -> result

-- | external link when True
hyperlink ::
  WebpageHakyllDataExchangeProtocol protocol =>
  (Text -> WebpageBody protocol () -> WebpageBody protocol ())
hyperlink url
  | isExternal $ T.unpack url = a_ (href_ url : newTabAttr)
  | otherwise = a_ [href_ url]

newTabAttr :: [Attribute]
newTabAttr = [target_ "_blank", rel_ "noreferrer noopener"]

hyperlinkEcho ::
  WebpageHakyllDataExchangeProtocol protocol =>
  (Text -> WebpageBody protocol ())
hyperlinkEcho url = hyperlink url $ toWebpageBody url

hyperlinkGitHub ::
  WebpageHakyllDataExchangeProtocol protocol =>
  (Text -> WebpageBody protocol ())
hyperlinkGitHub repo =
  hyperlink (https "github.com/" <> repo) $ toWebpageBody repo

hyperlinkList ::
  (Foldable t, WebpageHakyllDataExchangeProtocol protocol) =>
  (t Link -> WebpageBody protocol ())
hyperlinkList ls =
  ul_ $
    mapM_
      ( \(path, desc) ->
          li_ $ hyperlink (T.pack path) (toWebpageBodyRaw desc)
      )
      ls

-- code ::
--   (MonadReader env m, HasHeaderLevel env, Term [Attribute] (m a -> m a), Term (m a) (m a)) =>
--   (m a -> m a)
code ::
  WebpageHakyllDataExchangeProtocol protocol =>
  (WebpageBody protocol () -> WebpageBody protocol ())
code = code_
