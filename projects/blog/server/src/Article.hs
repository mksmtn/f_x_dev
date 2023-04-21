module Article (Article(..), view) where

import Data.Aeson (FromJSON, ToJSON)
import Data.String (IsString(fromString))
import qualified Data.Text as T
import GHC.Generics
import ArticlePreview
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Internal (preEscapedText)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import CMark (commonmarkToHtml)

data Article = Article { preview :: ArticlePreview
                       , content :: String } deriving (Generic, Show)

instance FromJSON Article
instance ToJSON Article


view :: Article -> (String, H.Html)
view article@(Article { preview = (ArticlePreview { title }) }) =
  ("f(x) | " ++ title, articleView article)


articleView :: Article -> H.Html
articleView (Article { preview, content }) =
  H.article ! A.class_ "container pt-4 pb-5 article" $ do
    metaView preview
    contentView content


metaView :: ArticlePreview -> H.Html
metaView (ArticlePreview { minsToRead, publishedAt }) =
  H.span $ do
    fromString . concat $ [ posixSecondsToMoscowDate publishedAt, " · ", printMinsToRead minsToRead ]


contentView :: String -> H.Html
contentView content =
  H.div . preEscapedText . (commonmarkToHtml []) . T.pack $ content
