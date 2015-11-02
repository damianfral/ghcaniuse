{-# LANGUAGE DeriveGeneric #-}

module GHCanIUse.Types where

import           BasicPrelude
import           Data.Aeson
import           Data.Text          (pack)
import           Data.Time.Calendar (Day, showGregorian)
import           GHC.Generics
import           Text.Parsec

data GHCRelease = GHCRelease
    { releaseDate    :: Day
    , releaseVersion :: (Int, Int, Int)
    } deriving (Show, Generic, Ord)

instance Eq GHCRelease where
    (GHCRelease _ (a,b,_)) == (GHCRelease _ (x,y,_)) = [a,b] == [x,y]
    _ == _ = False

instance ToJSON Day where
    toJSON = String . pack . showGregorian

instance ToJSON GHCRelease

type Parser = Parsec String ()

type URL = Text
type ReleasesMap = HashMap Text [GHCRelease]
type DocLinksMap = HashMap (Text, URL) [GHCRelease]
