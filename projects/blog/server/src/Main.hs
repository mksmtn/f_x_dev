{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import CMark
import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat)
import Data.Text.Lazy as LText
import Data.Text as Text
import System.IO

main = scotty 3000 $
    get "/:file-name" $ do
        fileName <- param "file-name"
        let filePath = "projects/blog/server/articles/" ++ fileName ++ ".md"
        content <- liftIO $ readFile $ filePath
        let textContent = Text.pack content
        let htmlText = commonmarkToHtml [] textContent
        fullHtml <- liftIO $ wrapHtml $ htmlText
        let lazyHtmlText = LText.fromStrict fullHtml
        html lazyHtmlText


wrapHtml :: Text.Text -> IO Text.Text
wrapHtml article =
    do
        prismCss <- readFile "projects/blog/server/assets/prism.css"
        prismJs  <- readFile "projects/blog/server/assets/prism.js"
        let list = [ Text.pack "<!DOCTYPE html><html><head><style>"
                   , Text.pack prismCss
                   , Text.pack "</style></head><body>"
                   , article
                   , Text.pack "<script>"
                   , Text.pack prismJs
                   , Text.pack "</script>"
                   , Text.pack "</body>"
                   , Text.pack "</html>"
                   ]
        let text = Text.concat list
        return text
