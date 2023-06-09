{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHCanIUse.Utils where

import BasicPrelude hiding (isPrefixOf)
import Data.Text (index, isPrefixOf, pack, unpack)
import GHCanIUse.Types
import Turtle

showNix :: GHCRelease -> Text
showNix (GHCRelease _ (a, b, c)) =
  pack $
    "haskell.compiler.ghc" <> mconcat (show <$> [a, b, c])

getLanguageExtensions :: GHCRelease -> IO [Text]
getLanguageExtensions r = lines <$> shellOutput
  where
    command =
      unwords
        [ "nix-shell -I ~/devel/ -p ",
          showNix r,
          "--command 'ghc --supported-languages'"
        ]
    shellOutput = strict $ inshell command mempty

isNoExtension :: Text -> Bool
isNoExtension ext
  | isPrefixOf "No" ext && (index ext 2 `elem` (['A' .. 'Z'] :: String)) = True
  | otherwise = False

filterExtensions :: [Text] -> [Text]
filterExtensions = filter $ not . isNoExtension

splitEvery :: Int -> [a] -> [[a]]
splitEvery i = unfoldr go
  where
    go [] = Nothing
    go x = Just $ splitAt i x

ghcUserGuideURL (GHCRelease _ (x, y, z)) =
  mconcat
    [ "https://downloads.haskell.org/~ghc/",
      intercalate "." $ pack . show <$> [x, y, z],
      "/docs/html/users_guide/"
    ]

ghcFlagReferenceURL = (<> "flag-reference.html") . ghcUserGuideURL
