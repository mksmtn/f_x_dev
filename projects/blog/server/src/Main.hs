{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty (setHeader, scottyOpts, get, json, raise, raw, param, Options(Options), verbose, settings)
import Network.Wai.Handler.Warp (defaultSettings, setPort, setHost)
import Data.Streaming.Network.Internal (HostPreference(Host))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson (decode)
import Data.Maybe (fromMaybe)
import Data.String (IsString(fromString))
import qualified Data.List as List
import System.Environment (lookupEnv)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Home
import qualified About
import Article
import ArticlePreview
import Consts
import qualified Contacts
import CustomAttributes (integrity, crossorigin)

main :: IO ()
main = do
    portString <- lookupEnv "PORT"
    let port :: Int
        port = read (fromMaybe "3000" portString)
    hostString <- lookupEnv "HOST"
    let host = fromMaybe "localhost" hostString
    putStrLn $ "Host: " ++ host
    let settings = (setHost (Host host)) (setPort port defaultSettings)
    let opts = Options { verbose = 1, settings = settings }

    let render content = do
        setHeader "Cache-Control" "no-store, no-cache, must-revalidate"
        setHeader "Content-Type" "text/html; charset=utf-8"
        raw . renderHtml . wrapContent $ content

    scottyOpts opts $ do

        get "/" $ do
            articlePreviewList <- liftIO loadArticlePreviews
            case articlePreviewList of
                Just list ->
                    render . Home.view $ list
                Nothing ->
                    raise "Error parsing article list"

        get "/articles" $ do
            articlePreviewList <- liftIO loadArticlePreviews
            case articlePreviewList of
                Just list ->
                    json (HashMap.singleton ("data" :: Text.Text) list)
                Nothing ->
                    raise "Error parsing article list"

        get "/articles/:file-name" $ do
            fileName <- param "file-name"
            let filePath = "projects/blog/server/articles/" ++ fileName ++ ".md"
            markdownText <- liftIO $ readFile filePath
            content <- liftIO $ BSL.readFile articlePreviewListPath
            let articlePreviewList = decode content :: Maybe [ArticlePreview]
            case articlePreviewList of
                Just list ->
                    case (List.find (\(ArticlePreview {slug = slug}) -> slug == fileName) list) of
                        Just preview ->
                            render . Article.view $ article
                            where
                                article = Article { preview = preview, content = markdownText }
                        Nothing ->
                            raise "Not Found"
                Nothing ->
                    raise "Error parsing previews"

        get "/about" $ do
            render About.view
        
        get "/contacts" $ do
            render Contacts.view
 


loadArticlePreviews :: IO (Maybe [ArticlePreview])
loadArticlePreviews = do
    content <- BSL.readFile articlePreviewListPath
    return $ decode content


wrapContent :: (String, H.Html) -> H.Html
wrapContent (title, body) =
    H.docTypeHtml $ do
        H.head $ do
            H.title (H.toHtml title)
            H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
            H.meta ! A.name "google-site-verification" ! A.content "bI9akRHitJ4Ate9XY-zDTXozQGMV0k1YjcCcXPxIFtI"
            H.link ! A.href (fromString bootstrapCssLocation) ! A.rel "stylesheet" ! integrity (fromString bootstrapCssSha) ! crossorigin "anonymous"
            H.link ! A.href "/assets/css/prism.css" ! A.rel "stylesheet"
            H.link ! A.href "/assets/css/article.css" ! A.rel "stylesheet"
            H.script $ do
                (fromString . unlines)
                    [ "const metrikaCounter = 90878380;"
                    , "(function (m, e, t, r, i, k, a) {"
                    , "m[i] = m[i] || function () { (m[i].a = m[i].a || []).push(arguments) };"
                    , "m[i].l = 1 * new Date();"
                    , "for (var j = 0; j < document.scripts.length; j++) { if (document.scripts[j].src === r) { return; } }"
                    , "k = e.createElement(t), a = e.getElementsByTagName(t)[0], k.async = 1, k.src = r, a.parentNode.insertBefore(k, a)"
                    , "})"
                    , "(window, document, 'script', 'https://mc.yandex.ru/metrika/tag.js', 'ym');"
                    , ""
                    , "ym(metrikaCounter, 'init', {"
                    , "defer: true,"
                    , "clickmap: true,"
                    , "trackLinks: true,"
                    , "accurateTrackBounce: true,"
                    , "webvisor: true"
                    , "});"
                    ]
            H.noscript $ do
                H.div $ do
                    H.img ! A.src "https://mc.yandex.ru/watch/90878380" ! A.style "position:absolute; left:-9999px;" ! A.alt ""

        H.body $ do
            H.div ! A.class_ "d-flex flex-column" ! A.style "min-height: 100vh" $ do
                H.div $ do
                    header
                H.div ! A.class_ "flex-grow-1 text-body bg-body" $ do
                    body
                footer
            H.script ! A.src (fromString bootstrapJsLocation) ! integrity (fromString bootstrapJsSha) ! crossorigin "anonymous" $ do
                mempty
            H.script ! A.src "/assets/js/prism.js" $ do
                mempty


footer :: H.Html
footer =
    H.footer ! A.class_ "bg-primary-subtle pb-2 pt-3" $ do
        H.div ! A.class_ "container" $ do
            H.ul ! A.class_ "list-unstyled" $ do
                mapM_ footerLink footerLinksList


type NavLinkUrl = String
type NavLinkLabel = String
type NavLink = (NavLinkUrl, NavLinkLabel)


footerLinksList :: [NavLink]
footerLinksList =
    [ ("/", "Главная")
    , ("/about", "О сайте")
    , ("/contacts", "Контакты")
    , (tgChannelLink, "Телеграм")
    ]


footerLink :: NavLink -> H.Html
footerLink (url, label) =
    H.li ! A.class_ "mb-1" $ do
        H.a ! A.class_ "text-light-emphasis" ! A.href (fromString url) $ do
            H.toHtml label


header :: H.Html
header =
    H.header ! A.class_ "py-1" ! A.style "background: var(--bs-purple)" $ do
        H.div ! A.class_ "container" $ do
            H.a ! A.href "/" ! A.class_ "text-light text-decoration-none" $ do
                "f(x)"
