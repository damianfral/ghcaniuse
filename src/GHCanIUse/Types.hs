{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHCanIUse.Types where

import qualified Data.Map as Map
import Data.Set as Set
import Data.Text (pack)
import qualified Data.Text as T
import GHC.Generics
import Relude
import Text.URI

-- | Represents a version of GHC.
data GHCVersion = GHCVersion {major :: Int, minor :: Int, patch :: Int}
  deriving (Show, Generic, Eq, Ord)

instance Hashable GHCVersion

-- | A set of all GHC versions from 7.0.0 to 12.10.10
allGHCVersions :: Set GHCVersion
allGHCVersions =
  Set.fromList
    [ GHCVersion {..}
      | major <- [7 .. 12],
        minor <- [0, 2 .. 12],
        patch <- [0 .. 9]
    ]

-- | Get the display string for a GHC version.
displayVersion :: GHCVersion -> Text
displayVersion (GHCVersion x y z) =
  pack $ "GHC-" <> intercalate "." (show <$> [x, y, z])

--------------------------------------------------------------------------------

-- | Represents a language extension.
newtype LanguageExtension = LanguageExtension {unLanguageExtension :: Text}
  deriving stock (Show, Generic, Ord, Eq)

-- | Documentation URL for language extensions.
newtype LanguageExtensionsDocs = LanguageExtensionsDoc
  {unLanguageExtensionsDocs :: Map LanguageExtension (Maybe URI)}
  deriving stock (Show, Generic, Ord, Eq)
  deriving newtype (Semigroup, Monoid)

-- | Check if an extension is a negative form (NoOverlodadedString)
isNoExtension :: Text -> Bool
isNoExtension "NoImplicitPrelude" = False
isNoExtension ext = T.isPrefixOf "No" ext && (T.index ext 2 `elem` ['A' .. 'Z'])

-- | Filter out negative form extensions.
filterExtensions :: Set LanguageExtension -> Set LanguageExtension
filterExtensions = Set.filter $ not . isNoExtension . unLanguageExtension

--------------------------------------------------------------------------------

-- | Represents a release of GHC.
data GHCRelease = GHCRelease
  { releaseVersion :: GHCVersion,
    releaseLanguages :: LanguageExtensionsDocs,
    releaseWeb :: URI
  }
  deriving (Show, Generic, Ord, Eq)

-- | Get the display string for a GHC release.
displayRelease :: GHCRelease -> Text
displayRelease = displayVersion . releaseVersion

-- | Get the display string for a language extension at a specific GHC version.
displayLangAtVersion :: LanguageExtension -> GHCVersion -> Text
displayLangAtVersion lang version =
  unLanguageExtension lang <> "@" <> displayVersion version

-- | Get the display string for a language extension at a specific GHC version.
supportsExtension :: LanguageExtension -> GHCRelease -> Bool
supportsExtension extension =
  isJust . Map.lookup extension . unLanguageExtensionsDocs . releaseLanguages

-- | Get the display string for a language extension at a specific GHC version.
getExtensionnLanguageDoc :: LanguageExtension -> GHCRelease -> Maybe URI
getExtensionnLanguageDoc extension =
  join . Map.lookup extension . unLanguageExtensionsDocs . releaseLanguages

-- | Get all language extensions present in a set of GHC releases.
getAllExtensions :: Set GHCRelease -> [LanguageExtension]
getAllExtensions = Map.keys . unLanguageExtensionsDocs . foldMap releaseLanguages

--------------------------------------------------------------------------------

-- | A map of language extensions to sets of GHC releases supporting them.
type LanguageExtensionsMap = Map LanguageExtension (Set GHCRelease)

-- | Convert a set of GHC releases to a map of language extensions to supported releases.
ghcReleasesToExtensionsMap :: Set GHCRelease -> LanguageExtensionsMap
ghcReleasesToExtensionsMap releases =
  foldMap getReleasesSupportingExtension allExtensions
  where
    allExtensions = getAllExtensions releases
    getReleasesSupportingExtension ext =
      Map.singleton ext $ Set.filter (supportsExtension ext) releases
