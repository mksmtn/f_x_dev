{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty (setHeader, status, middleware, scottyOpts, get, json, raise, html, param, Options(Options), verbose, settings)
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import Network.Wai.Handler.Warp (defaultSettings, setPort, setHost)
import Data.Streaming.Network.Internal (HostPreference(Host))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as LText
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import System.IO
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Maybe (fromMaybe)
import qualified Data.List as List
import System.Environment (lookupEnv)
import GHC.Generics

data ArticlePreview = ArticlePreview { title :: Text.Text, publishedAt :: Int, minsToRead :: Int, slug :: Text.Text } deriving (Generic, Show)

instance FromJSON ArticlePreview
instance ToJSON ArticlePreview

data Article = Article { preview :: ArticlePreview, content :: String } deriving (Generic, Show)

instance FromJSON Article
instance ToJSON Article

articlePreviewListPath = "projects/blog/server/articles/article-preview-list.json"

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

        get "/articles" $ do
            content <- liftIO $ BSL.readFile articlePreviewListPath
            let articlePreviewList = decode content :: Maybe [ArticlePreview]
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
                    case (List.find (\_ -> True) list) of
                        Just preview ->
                            json response
                            where
                                article = Article { preview = preview, content = markdownText }
                                response = HashMap.singleton ("data" :: Text.Text) article
                        Nothing ->
                            raise "Not Found"
                Nothing ->
                    raise "Error parsing previews"
