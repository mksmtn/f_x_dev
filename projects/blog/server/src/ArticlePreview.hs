module ArticlePreview (
  ArticlePreview(..),
  printMinsToRead,
  posixSecondsToMoscowDate,
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Data.Time as Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)


data ArticlePreview = ArticlePreview { title :: String
                                     , publishedAt :: Int
                                     , minsToRead :: Int
                                     , slug :: String
                                     , private :: Bool
                                     } deriving (Generic, Show)

instance FromJSON ArticlePreview
instance ToJSON ArticlePreview


mskZone :: TimeZone
mskZone =
  TimeZone { timeZoneMinutes = 3 * 60
           , timeZoneSummerOnly = False
           , timeZoneName = "Europe/Moscow"
           }


-- todo: remove year if year is equal to the current year
-- todo: respect browser time zone
-- todo: respect browser language
posixSecondsToMoscowDate :: Int -> String
posixSecondsToMoscowDate timestamp =
  Time.formatTime
    Time.defaultTimeLocale
    "%d %b %Y"
    (Time.utcToLocalTime mskZone (posixSecondsToUTCTime (realToFrac (timestamp `div` 1000))))


printMinsToRead :: Int -> String
printMinsToRead minsToRead = (show minsToRead) ++ " мин"
