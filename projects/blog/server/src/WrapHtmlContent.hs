module WrapHtmlContent (wrapHtmlContent) where

import Data.String (IsString(fromString))
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import Consts (tgChannelLink, githubLink, bootstrapCssLocation, bootstrapCssSha, bootstrapJsLocation, bootstrapJsSha)
import CustomAttributes (integrity, crossorigin)

wrapHtmlContent :: (String, H.Html) -> H.Html
wrapHtmlContent (title, body) =
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
    , (githubLink, "GitHub")
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
