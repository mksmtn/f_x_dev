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
        let lazyHtmlText = LText.fromStrict htmlText
        html lazyHtmlText
