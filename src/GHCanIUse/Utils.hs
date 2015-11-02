module GHCanIUse.Utils where

import           BasicPrelude    hiding (isPrefixOf)
import           Data.Text       (index, isPrefixOf)
import           GHCanIUse.Types
import           Turtle


showNix :: GHCRelease -> Text
showNix (GHCRelease _ (a,b,c)) =
    "haskell.compiler.ghc" <> (mconcat $ show <$> [a,b,c])

getLanguageExtensions :: GHCRelease -> IO [Text]
getLanguageExtensions r = lines <$> shellOutput
    where
        command = unwords
                      [ "nix-shell -I ~/devel/ -p "
                      , showNix r
                      , "--command 'ghc --supported-languages'" ]
        shellOutput = strict $ inshell command mempty

isNoExtension :: Text -> Bool
isNoExtension ext
    | isPrefixOf "No" ext && (index ext 2 `elem` (['A' .. 'Z']::String)) = True
    | otherwise = False

filterExtensions :: [Text] -> [Text]
filterExtensions = filter $ not . isNoExtension
