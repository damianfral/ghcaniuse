{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Main where

import           BasicPrelude              hiding (isPrefixOf, lookup, try
                                                  ,(<|>))
import           Control.Monad.Trans.State
import           Data.Aeson
import qualified Data.ByteString.Lazy      as BS
import           Data.HashMap.Lazy         hiding (filter, (!))
import           Data.Text                 (index, isPrefixOf, pack)
import           Data.Time.Calendar        (showGregorian)
import           GHCRelease
import           Lucid
import           Lucid.Base
import           Turtle.Prelude


showNix :: GHCRelease -> Text
showNix (GHCRelease _ (a,b,c)) =
    "haskell.compiler.ghc" <> (mconcat $ show <$> [a,b,c])

getLanguageExtensions :: GHCRelease -> IO [Text]
getLanguageExtensions r = lines <$> shellOutput
    where
        command = mconcat
                      [ "nix-shell -I ~/devel/ -p "
                      , showNix r
                      , " --command 'ghc --supported-languages'" ]
        shellOutput = strict $ inshell command mempty

isNoExtension :: Text -> Bool
isNoExtension ext
    | isPrefixOf "No" ext && (index ext 2 `elem` (['A' .. 'Z']::String)) = True
    | otherwise = False

filterExtensions :: [Text] -> [Text]
filterExtensions = filter $ not . isNoExtension

(!) a b = a `with` [b]

type Table = HashMap Text [GHCRelease]

generateHTML :: Table -> Html ()
generateHTML table = html_ $ do
    head_ $ do
        title_   $ "GHCanIUse"
        style_' "https://fonts.googleapis.com/css?family=Roboto"
        style_' "style.css"
        script_ [type_ "text/javascript"]
            ("(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)})(window,document,'script','//www.google-analytics.com/analytics.js','ga');ga('create', 'UA-42041306-4', 'auto');ga('send', 'pageview');"::Text)

    body_ $ do
        table_ ! id_ "ghc-extensions" $ do
            thead_ $ tr_ $ do
                td_ $ "extensions"
                mapM_ td_ $ displayRelease <$> allReleases
            tbody_ $
                forM_ allExtensions $ \extension ->
                    tr_ $ do
                        td_ $ toHtml extension
                        forM_ allReleases $ \release ->
                            go $ elem release <$> lookup extension table

        script_' "tablesort.min.js"
        script_ [type_ "text/javascript"]
            ("new Tablesort(document.getElementById('ghc-extensions'));"::Text)

    where
        allExtensions  = sort $ filterExtensions $ keys table
        allReleases    = reverse $ sort $ nub $ mconcat $ snd <$> toList table
        go (Just True) = td_ ! class_ "supported"     $ "✓"
        go _           = td_ ! class_ "not-supported" $ "-"

        displayRelease :: GHCRelease -> Html ()
        displayRelease (GHCRelease d (x,y,z)) =
            toHtml $ "GHC-" <> intercalate "." (show <$> [x,y])

        script_' :: Text -> Html ()
        script_' x = makeElement "script" ! type_ "text/javascript" ! src_ x $ ""
        style_' hrefV = link_   [ type_ "text/css"
                                , rel_ "stylesheet"
                                , href_ hrefV ]

main :: IO ()
main = do
    scrapGHCReleases
        >>= pure . nub . catMaybes . fmap (uncurry parseGHCReleases)
        >>= (flip execStateT mempty . mapM_ process)
        >>= \x -> do renderToFile "index.html" $ generateHTML x
                     BS.writeFile "ghc-extensions.json" $ encode x

    where
        process :: GHCRelease -> StateT Table IO ()
        process x = do
            langExts <- liftIO $ getLanguageExtensions x
            forM_ langExts $ \lang -> modify $ insertWith mappend lang [x]
