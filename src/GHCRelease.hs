{-# LANGUAGE DeriveGeneric #-}

module GHCRelease
    (GHCRelease(..), scrapGHCReleases, parseGHCReleases)
where

import           Data.Aeson
import           Data.Maybe
import           Data.Text           (pack)
import           Data.Time.Calendar  (Day, fromGregorian, showGregorian)
import           GHC.Generics
import           Text.HTML.Scalpel
import           Text.Parsec


data GHCRelease = GHCRelease
    { releaseDate    :: Day
    , releaseVersion :: (Int, Int, Int)
    } deriving (Show, Eq, Generic, Ord)

instance ToJSON Day where
    toJSON = String . pack . showGregorian

instance ToJSON GHCRelease

scrapGHCReleases :: IO ([(String, String)])
scrapGHCReleases = fromMaybe [] <$> scrapeURL "https://www.haskell.org/ghc/" versionsScrapper

versionsScrapper :: Scraper String [(String, String)]
versionsScrapper = chroot ("td" @: [hasClass "rightpane"] // "dl") $ do
    r <- texts $ "dt"
    v <- texts $ "dd"
    return $ zip r v

type Parser = Parsec String ()

numberParser :: Parser Int
numberParser = many1 digit >>= \d -> return (read d :: Int)

monthParser :: Parser Int
monthParser = foldl1 (<|>) $  zipWith ((<*)) numbers months
    where
    numbers = pure <$> [1..]
    months  = try . string <$>
        [ "January", "February", "March" , "April", "May", "June"
        , "July", "Agost", "September", "October", "November", "December" ]

eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right x) = Just x

parseReleaseDate :: String -> Maybe Day
parseReleaseDate = eitherToMaybe . runParser p () ""
    where
        p = do
            day   <- numberParser
            string " "
            month <- monthParser
            string " "
            year  <- numberParser
            return $ fromGregorian (fromIntegral year) month day

parseReleaseNumber :: String -> Maybe (Int, Int, Int)
parseReleaseNumber = eitherToMaybe . runParser p () ""
    where
        p = do
            string "GHC "
            a <- numberParser
            string "."
            b <- numberParser
            string "."
            c <- numberParser
            return (a,b,c)

parseGHCReleases :: String -> String -> Maybe GHCRelease
parseGHCReleases date code = GHCRelease <$> parseReleaseDate date
                                        <*> parseReleaseNumber code
