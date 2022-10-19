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
        let lazyHtmlText = LText.fromStrict htmlText
        html lazyHtmlText
