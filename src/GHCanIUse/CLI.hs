{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHCanIUse.CLI where

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (pack, unpack)
import qualified Data.Text as T
import GHCanIUse.Render (fromUtf8BS, generatePage)
import GHCanIUse.Scraper
import GHCanIUse.Types as G
import Lucid
import Options.Generic
import Relude
import System.Directory hiding (makeAbsolute)
import System.FilePath ((</>))
import Text.URI
import Text.URI.QQ (uri)

data Options w = Options
  { directory :: w ::: FilePath <!> "./result/",
    noDocs :: w ::: Bool
  }
  deriving (Generic)

instance ParseRecord (Options Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

runCLI :: IO ()
runCLI = do
  options <- unwrapRecord "GHCanIUse"
  releases <- getGHCReleases options
  renderToFile "index.html" $ generatePage $ ghcReleasesToExtensionsMap releases

--------------------------------------------------------------------------------

getGHCReleases :: Options Unwrapped -> IO (Set GHCRelease)
getGHCReleases = getGHCReleasesFor allGHCVersions

getGHCReleasesFor :: Set GHCVersion -> Options Unwrapped -> IO (Set GHCRelease)
getGHCReleasesFor releases dir =
  foldMap (processVersion dir) (Set.toList releases)

processVersion :: Options Unwrapped -> GHCVersion -> IO (Set GHCRelease)
processVersion Options {..} version = do
  let fp = getFilePath version
  languages <- getLanguagesFromFile fp
  urls <- if noDocs then mempty else getAllDocURLsIO $ ghcUserGuideURL version
  let languageDocs = scrapeGHCExtensionsDoc version languages urls
  -- Log
  mapM_ logEntry (Map.toList $ unLanguageExtensionsDocs languageDocs)
  pure $ Set.singleton $ makeGHCRelease version languageDocs
  where
    getFilePath = (directory </>) . unpack . getLanguagesFileName
    logEntry (extension, url) =
      putStrLn . unpack $
        displayLangAtVersion extension version <> " -> " <> maybe "" render url

getLanguagesFromFile :: FilePath -> IO (Set LanguageExtension)
getLanguagesFromFile fp = do
  fileExists <- doesFileExist fp
  if fileExists
    then do
      extensionsText <- T.lines . fromUtf8BS <$> BS.readFile fp
      let extensions = LanguageExtension <$> extensionsText
      pure $ filterExtensions $ Set.fromList extensions
    else pure mempty

getLanguagesFileName :: GHCVersion -> Text
getLanguagesFileName version =
  T.toLower $ fold [displayVersion version, "-languages.txt"]

makeGHCRelease :: GHCVersion -> LanguageExtensionsDocs -> GHCRelease
makeGHCRelease releaseVersion releaseLanguages =
  GHCRelease
    { releaseVersion,
      releaseLanguages,
      releaseWeb = ghcUserGuideURL releaseVersion
    }

ghcUserGuideURL :: GHCVersion -> URI
ghcUserGuideURL v@(GHCVersion {..}) =
  fromMaybe baseURI $
    emptyURI
      { uriScheme = Nothing,
        uriAuthority = Left False,
        uriPath = sequence pathsNE >>= \paths -> pure (True, paths),
        uriQuery = [],
        uriFragment = Nothing
      }
      `relativeTo` baseURI
  where
    versionNumbers = [major, minor, patch]
    ghcPath = mkPathPiece $ pack "~ghc"
    versionPath = mkPathPiece $ pack $ intercalate "." $ show <$> versionNumbers
    docsPath = mkPathPiece "docs"
    htmlPath = mkPathPiece "html"
    usersGuidePath = mkPathPiece "users_guide"
    baseURI = [uri|https://downloads.haskell.org/|]
    pathsNE =
      if v <= GHCVersion 9 2 4
        then ghcPath :| [versionPath, docsPath, htmlPath, usersGuidePath]
        else ghcPath :| [versionPath, docsPath, usersGuidePath]
