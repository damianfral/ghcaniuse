{-# LANGUAGE NoOverloadedStrings #-}

module GHCanIUse.Scrapper
    ( scrapGHCReleases
    , scrapDoc
    , scrapDocLatest)
where

import           BasicPrelude       hiding (try, (<|>))
import           Data.Maybe
import           Data.Text          (pack, unpack)
import           Data.Time.Calendar (Day, fromGregorian, showGregorian)
import           GHC.Generics
import           GHCanIUse.Types
import           Text.HTML.Scalpel
import           Text.Parsec
-- import Data.List

-- Releases

scrapGHCReleases :: IO [GHCRelease]
scrapGHCReleases = do
    allReleases <- fromMaybe [] <$> scrapeURL "https://www.haskell.org/ghc/" versionsScrapper
    return $ nub . catMaybes . fmap (uncurry parseGHCReleases) $ allReleases

versionsScrapper :: Scraper String [(String, String)]
versionsScrapper = chroot ("td" @: [hasClass "rightpane"] // "dl") $ do
    r <- texts $ "dt"
    v <- texts $ "dd"
    return $ zip r v

numberParser :: Parser Int
numberParser = many1 digit >>= \d -> return ((read $ pack d) :: Int)

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

extensionIndexScrapper :: Scraper String [(String, String)]
extensionIndexScrapper = fmap mconcat $ chroots "tbody" $ do
    flags <- texts $ "tr" // "td" // "code"
    descriptions <- attrs "href" $ "tr" // "td" // "a"
    return $ zip (drop 2 <$> flags) descriptions


-- Docs
scrapDoc (GHCRelease _ (x,y,z)) = scrapeURL url $ extensionIndexScrapper
    where url = mconcat [ "https://downloads.haskell.org/~ghc/"
                        , intercalate "." $ unpack.show <$> [x,y,z]
                        , "/docs/html/users_guide/flag-reference.html" ]

scrapDocLatest = scrapeURL url $ extensionIndexScrapper
    where url = mconcat [ "https://downloads.haskell.org/~ghc/"
                        , "latest"
                        , "/docs/html/users_guide/flag-reference.html" ]
