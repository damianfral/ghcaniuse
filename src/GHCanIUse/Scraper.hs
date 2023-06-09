{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHCanIUse.Scraper where

import Control.Concurrent.Async (forConcurrently_)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text hiding (filter)
import qualified Data.Text as T hiding (filter)
import GHCanIUse.Types
import Relude
import Text.Casing
import Text.HTML.Scalpel
import qualified Text.HTML.Scalpel as S
import Text.URI (URI, mkURI, render)

type Scalpel = Scraper Text

scrapeAllURLs :: Scalpel (Set Text)
scrapeAllURLs = do
  hrefs <- S.attrs "href" "a"
  pure $ Set.filter ("" /=) $ Set.fromList hrefs

-- | Scrape the documentation for GHC language extensions.
scrapeGHCExtensionsDoc ::
  GHCVersion -> Set LanguageExtension -> Set URI -> LanguageExtensionsDocs
scrapeGHCExtensionsDoc v extensions urls =
  foldMap (scrapeGHCExtensionDoc v urls) extensions

-- | Scrape the documentation for a specific GHC language extension.
scrapeGHCExtensionDoc ::
  GHCVersion -> Set URI -> LanguageExtension -> LanguageExtensionsDocs
scrapeGHCExtensionDoc v urls extension
  | v < GHCVersion 8 0 0 = singleDoc ghc7doc
  | v < GHCVersion 8 2 4 = singleDoc ghc8doc
  | otherwise = LanguageExtensionsDoc $ Map.singleton extension ghc9doc
  where
    singleDoc = LanguageExtensionsDoc . Map.singleton extension
    filteredUrls = Set.filter isGHCExtensionURL urls
    ext = unLanguageExtension extension
    ghc7ExtensionHash = pack $ "#" <> toKebab (fromHumps $ unpack ext)
    ghc8ExtensionHash = "#ghc-flag--X" <> ext
    ghc9ExtensionHash = "#extension-" <> ext
    isGHC7ExtensionURL url = T.isSuffixOf ghc7ExtensionHash (render url)
    isGHC8ExtensionURL url = T.isSuffixOf ghc8ExtensionHash (render url)
    isGHC9ExtensionURL url = T.isSuffixOf ghc9ExtensionHash (render url)
    predicates = [isGHC7ExtensionURL, isGHC8ExtensionURL, isGHC9ExtensionURL]
    isGHCExtensionURL = getAny . foldMap (Any .) predicates
    ghc7doc = findURL isGHC7ExtensionURL filteredUrls
    ghc8doc = findURL isGHC8ExtensionURL filteredUrls
    ghc9doc = findURL isGHC9ExtensionURL filteredUrls
    findURL f = listToMaybe . Set.toList . Set.filter f

-- | Scrape the documentation for a specific GHC language extension.
rootOf :: Text -> Text
rootOf url
  | T.isSuffixOf "/" url = url
  | otherwise =
      url & T.splitOn "/" & dropEnd' 1 & T.intercalate "/" & flip mappend "/"

dropEnd' :: Int -> [a] -> [a]
dropEnd' n = Relude.reverse . Relude.drop n . Relude.reverse

getAllDocURLsIO :: URI -> IO (Set URI)
getAllDocURLsIO url = do
  tvar <- newTVarIO mempty
  getAllURLs tvar url
  readTVarIO tvar

getAllURLs :: TVar (Set URI) -> URI -> IO ()
getAllURLs tvar url
  | urlText /= trimHash urlText = pure ()
  | otherwise = do
      scrappedURLS <- S.scrapeURL (unpack urlText) scrapeAllURLs
      case scrappedURLS of
        Nothing -> pure mempty
        Just urls -> do
          registeredURLs <- readTVarIO tvar
          let validUrls = Set.fromList $ mapMaybe makeAbsoluteURL $ Set.toList urls
          let newUrls = Set.difference validUrls registeredURLs
          mapM_ (putStrLn . unpack . render) newUrls
          atomically $ modifyTVar' tvar $ mappend newUrls
          forConcurrently_ newUrls $ getAllURLs tvar
  where
    trimHash = fst . T.breakOn "#"
    urlText = render url
    makeAbsoluteURL url'
      | T.isInfixOf "../" url' = Nothing
      | T.isInfixOf "/.." url' = Nothing
      | T.isInfixOf "mailto:" url' = Nothing
      | T.isPrefixOf "http://" url' = Nothing
      | T.isPrefixOf "https://" url' = Nothing
      | T.isPrefixOf urlText url' = mkURI url'
      | T.isPrefixOf "#" url' = mkURI $ urlText <> url'
      | otherwise = mkURI $ rootOf urlText <> url'
