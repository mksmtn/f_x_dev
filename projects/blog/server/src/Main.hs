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
import qualified Data.List as List
import System.Environment (lookupEnv)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Home
import qualified About
import Article
import ArticlePreview
import qualified Contacts
import Consts (articlePreviewListPath)
import WrapHtmlContent (wrapHtmlContent)

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
        raw . renderHtml . wrapHtmlContent $ content

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
