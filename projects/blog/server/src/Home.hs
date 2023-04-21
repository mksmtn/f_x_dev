module Home (view) where

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import Data.String (IsString(fromString))
import ArticlePreview

view :: [ArticlePreview] -> (String, H.Html)
view previews =
  let title = "f(x) | Последние статьи"
      content = H.div H.! A.class_ "container pb-5 pt-4" $ do
                  H.h2 H.! A.class_ "mb-3" $ do
                    "Последние статьи"
                  H.ol ! A.class_ "list-unstyled" $ do
                    mapM_ (H.li . articlePreview) previews
  in
    (title, content)


articlePreview :: ArticlePreview -> H.Html
articlePreview (ArticlePreview { slug, title, minsToRead, publishedAt }) =
  H.div ! A.class_ "mb-3" $ do
    H.a ! A.class_ "text-decoration-none text-body h6" ! A.href (fromString . ((++) "/articles/") $ slug) $ H.toHtml title
    H.div ! A.class_ "text-secondary" $ do
      H.toHtml (posixSecondsToMoscowDate publishedAt ++ " · " ++ (printMinsToRead minsToRead))
