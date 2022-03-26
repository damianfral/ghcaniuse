{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           BasicPrelude              hiding (isPrefixOf, lookup,
                                            (<|>))
import           Control.Monad.Trans.State
import           GHCanIUse
import           Lucid


main :: IO ()
main = do
    scrapeGHCReleases
        >>= (flip execStateT mempty . mapM_ processRelease)
        >>= renderToFile "public/index.html" . uncurry generateIndexPage
            -- forM_ (keys x) $ \ext ->
            --     renderToFile (unpack ("public/" <> ext <> ".html"))
            --                  (generateExtensionPage ext x y)
