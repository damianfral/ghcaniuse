{-# LANGUAGE DeriveGeneric #-}

module GHCanIUse.Types where

import           BasicPrelude
import           Data.Aeson
import           Data.Text          (pack)
import           Data.Time.Calendar (Day (..), showGregorian)
import           GHC.Generics
import           Text.Parsec

data GHCRelease = GHCRelease
    { releaseDate    :: Day
    , releaseVersion :: (Int, Int, Int)
    } deriving (Show, Generic, Ord)

instance Eq GHCRelease where
    (GHCRelease _ (a,b,_)) == (GHCRelease _ (x,y,_)) = [a,b] == [x,y]
    _ == _ = False

instance ToJSON GHCRelease

instance Hashable Day where
    hashWithSalt s (ModifiedJulianDay x) = s `hashWithSalt` x

instance Hashable GHCRelease

type Parser = Parsec String ()

type URL = String
type ReleasesMap = HashMap Text [GHCRelease]
type DocLinksMap = HashMap (Text, GHCRelease) URL
