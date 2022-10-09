{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty (status, middleware, scottyOpts, get, html, param, Options(Options), verbose, settings)
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import Network.Wai.Handler.Warp (defaultSettings, setPort, setHost)
import Data.Streaming.Network.Internal (HostPreference(Host))
import CMark (commonmarkToHtml)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as LText
import qualified Data.Text as Text
import System.IO
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)

main = do
    portString <- lookupEnv "PORT"
    let port :: Int
        port = read (fromMaybe "3000" portString)
    hostString <- lookupEnv "HOST"
    let host = fromMaybe "localhost" hostString
    putStrLn $ "Host: " ++ host
    let settings = (setHost (Host host)) (setPort port defaultSettings)
    let opts = Options { verbose = 1, settings = settings }
    scottyOpts opts $ do
    middleware $ staticPolicy (noDots >-> addBase "projects/blog/server/assets")
    get "/:file-name" $ do
        fileName <- param "file-name"
        let filePath = "projects/blog/server/articles/" ++ fileName ++ ".md"
        content <- liftIO $ readFile filePath
        let textContent = Text.pack content
        let htmlText = commonmarkToHtml [] textContent
        fullHtml <- liftIO $ wrapHtml htmlText
        let lazyHtmlText = LText.fromStrict fullHtml
        html lazyHtmlText
    get "/" $ do
        let content = Text.pack homePage
        htmlText <- liftIO $ wrapHtml content
        html . LText.fromStrict $ htmlText


homePage :: String
homePage =
    let link = (\(name, link) -> "<li><a href=" ++ link ++ ">" ++ name ++ "</a></li>")
        links = concatMap link [("Маппинг типов в Typescript на примере", "/all-strings-typescript-types-mapping"), ("Паттерн матчинг в Python", "/pattern-matching-in-python-01")]
    in  "<h1>f(x)</h1><nav><ol>" ++ links ++ "</ol></nav>"


wrapHtml :: Text.Text -> IO Text.Text
wrapHtml article =
    do
        prismCss <- readFile "projects/blog/server/assets/prism.css"
        prismJs  <- readFile "projects/blog/server/assets/prism.js"
        let list = [ Text.pack "<!DOCTYPE html><html><head><style>"
                   , Text.pack prismCss
                   , Text.pack "</style></head><body>"
                   , Text.pack "<nav><ul><li><a href=\"/\">Домой</a></li></ul></nav>"
                   , article
                   , Text.pack "<script>"
                   , Text.pack prismJs
                   , Text.pack "</script>"
                   , Text.pack "</body>"
                   , Text.pack "</html>"
                   ]
        let text = Text.concat list
        return text
