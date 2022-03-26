{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHCanIUse.Scraper
  ( scrapeGHCReleases,
    scrapeDoc,
    scrapeDocLatest,
  )
where

import BasicPrelude hiding ((<|>))
import Data.Text (pack, unpack)
import Data.Time.Calendar (Day, fromGregorian)
import GHCanIUse.Types
import GHCanIUse.Utils
import Text.HTML.Scalpel
import Text.Parsec

-- Releases
scrapeGHCReleases :: IO [GHCRelease]
scrapeGHCReleases = do
  allReleases <- fromMaybe [] <$> scrapeURL "https://www.haskell.org/ghc/" versionsScrapper
  return $ nub . mapMaybe (uncurry parseGHCReleases) $ allReleases

versionsScrapper :: Scraper String [(String, String)]
versionsScrapper = chroot ("td" @: [hasClass "rightpane"] // "dl") $ do
  r <- texts "dt"
  v <- texts "dd"
  return $ zip r v

numberParser :: Parser Int
numberParser = many1 digit >>= \d -> return ((read $ pack d) :: Int)

monthParser :: Parser Int
monthParser = foldl1 (<|>) $ zipWith (<*) numbers months
  where
    numbers = pure <$> [1 ..]
    months =
      try . string
        <$> [ "January",
              "February",
              "March",
              "April",
              "May",
              "June",
              "July",
              "Agost",
              "September",
              "October",
              "November",
              "December"
            ]

eitherToMaybe :: Either a1 a2 -> Maybe a2
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

parseReleaseDate :: String -> Maybe Day
parseReleaseDate = eitherToMaybe . runParser p () ""
  where
    p = do
      day <- numberParser
      void $ string " "
      month <- monthParser
      void $ string " "
      year <- numberParser
      return $ fromGregorian (fromIntegral year) month day

parseReleaseNumber :: String -> Maybe (Int, Int, Int)
parseReleaseNumber = eitherToMaybe . runParser p () ""
  where
    p = do
      void $ string "GHC "
      a <- numberParser
      void $ string "."
      b <- numberParser
      void $ string "."
      c <- numberParser
      return (a, b, c)

parseGHCReleases :: String -> String -> Maybe GHCRelease
parseGHCReleases date code =
  GHCRelease <$> parseReleaseDate date
    <*> parseReleaseNumber code

extensionIndexScrapper :: Scraper String [(String, String)]
extensionIndexScrapper = fmap mconcat $
  chroots "tbody" $ do
    allCells <- htmls $ "tr" // "td"
    let [col1, col2] = transpose $ take 2 <$> splitEvery 4 allCells
    return $ zip col1 col2
  where
    listOf2ToPair [x, y] = (x, y)

-- Docs

scrapeDoc :: GHCRelease -> IO [Maybe ([Char], String)]
scrapeDoc release = do
  cells <- scrapeURL (unpack $ ghcFlagReferenceURL release) extensionIndexScrapper
  return $ pairOfMaybesToMaybeOfPairs . go <$> fromMaybe [] cells
  where
    go (col1, col2) =
      ( fmap (drop 2) <$> scrapeStringLike col1 $ text "code",
        scrapeStringLike col2 $ attr "href" "a"
      )
    pairOfMaybesToMaybeOfPairs (Just a, Just b) = Just (a, b)
    pairOfMaybesToMaybeOfPairs _ = Nothing

scrapeDocLatest :: IO (Maybe [(String, String)])
scrapeDocLatest = scrapeURL url extensionIndexScrapper
  where
    url =
      mconcat
        [ "https://downloads.haskell.org/~ghc/",
          "latest",
          "/docs/html/users_guide/flag-reference.html"
        ]
