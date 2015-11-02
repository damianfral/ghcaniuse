module GHCanIUse
    (module G,
    processRelease)
    where

import           BasicPrelude
import           Control.Monad.Trans.State
import           Data.HashMap.Lazy         hiding (filter, (!))

import           GHCanIUse.Render          as G
import           GHCanIUse.Scrapper        as G
import           GHCanIUse.Types           as G
import           GHCanIUse.Utils           as G


processRelease :: GHCRelease -> StateT ReleasesMap IO ()
processRelease x = do
    langExts <- liftIO $ getLanguageExtensions x
    forM_ langExts $ \lang -> modify $ insertWith mappend lang [x]
