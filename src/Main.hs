{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           BasicPrelude              hiding (isPrefixOf, lookup, try,
                                            (<|>))
import           Control.Monad.Trans.State
import           Data.Aeson
import qualified Data.ByteString.Lazy      as BS
import           Data.HashMap.Lazy         hiding (filter, (!))
import           Data.Text                 (index, isPrefixOf, pack)
import           Data.Time.Calendar        (showGregorian)
import           GHCanIUse
import           Lucid
import           Turtle.Prelude

main :: IO ()
main = do
    scrapGHCReleases
        >>= (flip execStateT mempty . mapM_ processRelease)
        >>= \x -> do renderToFile "index.html" $ generateHTML x
                     BS.writeFile "ghc-extensions.json" $ encode x
