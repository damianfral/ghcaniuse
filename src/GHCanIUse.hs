{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHCanIUse
  ( module G,
    processRelease,
  )
where

import BasicPrelude hiding (insert, lookup)
import Control.Lens
import Control.Monad.Trans.State
import Data.HashMap.Lazy hiding (filter, (!))
import Data.Text (pack)
import GHCanIUse.Render as G
import GHCanIUse.Scraper as G
import GHCanIUse.Types as G
import GHCanIUse.Utils as G

processRelease :: GHCRelease -> StateT (ReleasesMap, DocLinksMap) IO ()
processRelease release = do
  langExts <- liftIO $ getLanguageExtensions release
  extDocs <- fromList . fmap (_1 %~ pack) . catMaybes <$> liftIO (scrapeDoc release)
  forM_ langExts $ \lang -> do
    _1 %= insertWith mappend lang [release]
    case lookup lang extDocs of
      Just url -> _2 %= insert (lang, release) url
      Nothing -> return ()
