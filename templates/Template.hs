-- | basic templates
module Template
  ( articleCommon,
    code,
    header,
    hyperlink,
    hyperlinkEcho,
    hyperlinkGitHub,
    hyperlinkHeader,
    hyperlinkList,
    paragraph,
    section,
    webpageCommon,
  )
where

-- Contex StringはStringのレコード(Map)みたいなもの!(暴論)

import Data.Binary
import Data.Functor.Identity
import Data.Generics.Product
import Data.Kind (Constraint)
import Data.String (IsString)
import Hakyll.Web.Html (isExternal)
import Lucid hiding (br_, doctypehtml_, input_, link_, meta_, required_)
import qualified Lucid (br_, doctypehtml_, input_, link_, meta_, required_)
import qualified Lucid.Base as Lucid (makeAttribute)
import qualified RIO.ByteString.Lazy as BL
import RIO.FilePath (takeDirectory)
import qualified RIO.List as L
import RIO.State
import qualified RIO.Text as T
import RIO.Time
import Share

doctypehtml_ :: WebpageBody protocol () -> WebpageBody protocol ()
doctypehtml_ = WebpageBody . Lucid.doctypehtml_ . runWebpageBody

link_, meta_, input_, br_ :: [Attribute] -> WebpageBody protocol ()
link_ = WebpageBody . Lucid.link_
meta_ = WebpageBody . Lucid.meta_
input_ = WebpageBody . Lucid.input_
br_ = WebpageBody . Lucid.input_

required_ :: Attribute
required_ = Lucid.required_ "required"

prefix_, property_, onkeypress_ :: Text -> Attribute
prefix_ = Lucid.makeAttribute "prefix"
property_ = Lucid.makeAttribute "property"
onkeypress_ = Lucid.makeAttribute "onkeypress"

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
      [ "if(event.key == 'Enter') {",
        "  this.form." <> name <> ".focus();",
        "  return false;",
        "}"
      ]

ctrlEnterSubmitForm :: Attribute
ctrlEnterSubmitForm =
  onkeydown_ $
    T.unlines
      [ "if(this.form.reportValidity() && event.ctrlKey && event.key == 'Enter') {",
        "  this.form.onsubmit();",
        "  return false;",
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
                hyperlinkHeader "/" "linguiniの✨ブログ✨"
                main_ $ view webpageBodyL webpage
                section "コメント欄" $ do
                  postDir <- T.pack . (\case '.' : p -> p; p -> p) . takeDirectory . view pathL <$> ask
                  form_
                    [ onsubmit_ $
                        T.unlines
                          [ "var form = this;",
                            "var message = form.message.value;",
                            "form.message.value='';",
                            "form.message.placeholder='コメント送信中...';",
                            "var req = new XMLHttpRequest ();",
                            "",
                            "req.open(",
                            "  'POST',",
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
                            "  if (this.readyState === XMLHttpRequest.DONE){",
                            "    if (this.status == 200) {",
                            "      form.message.placeholder='マサカリを投げる';",
                            "    } else {",
                            "      form.message.placeholder='コメント失敗';",
                            "    }",
                            "  }",
                            "}",
                            "",
                            "var query =",
                            "  'options[title]=' + encodeURIComponent('" <> view titleL webpage <> "')",
                            "  + '&options[path]=' + encodeURIComponent('" <> postDir <> "')",
                            "  + '&fields[name]=' + encodeURIComponent(form.name.value)",
                            "  + '&fields[message]=' + encodeURIComponent(message);",
                            "if (form.twitter.value) {",
                            "  query += '&fields[twitter]=' + encodeURIComponent(form.twitter.value);",
                            "}",
                            "req.send(query);",
                            "",
                            "return false;"
                          ]
                    ]
                    $ do
                      label_ $ "ハンドルネーム" <> input_ [name_ "name", type_ "text", required_, enterMoveFocusToNameInForm "twitter"]
                      label_ $ "twitter垢(任意)" <> input_ [name_ "twitter", type_ "text", placeholder_ "1inguini", pattern_ "^(\\w{1,15})$", maxlength_ "15", enterMoveFocusToNameInForm "message"]
                      textarea_ [name_ "message", rows_ "10", cols_ "100", placeholder_ "マサカリを投げる", required_, ctrlEnterSubmitForm] (pure ()) :: WebpageBody protocol ()
                      button_ [type_ "submit"] "送信" :: WebpageBody protocol ()
                footer_ $ "Copyright: © 2020 linguini. Site proudly generated by " <> hyperlink "http://jaspervdj.be/hakyll" "Hakyll" <> ". Visit the site repository from " <> hyperlink "https://github.com/1inguini/1inguini.github.io" "here" <> "."
        }

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

-- code ::
--   (MonadReader env m, HasHeaderLevel env, Term [Attribute] (m a -> m a), Term (m a) (m a)) =>
--   (m a -> m a)
code ::
  WebpageHakyllDataExchangeProtocol protocol =>
  (WebpageBody protocol () -> WebpageBody protocol ())
code = code_

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
