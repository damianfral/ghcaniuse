{-# LANGUAGE FlexibleContexts #-}

module GHCanIUse.Render where

import BasicPrelude hiding (lookup)
import           Lucid
import           Lucid.Base
import GHCanIUse.Types
import GHCanIUse.Utils
import           Data.HashMap.Lazy hiding (filter, (!))


(!) a b = a `with` [b]

generateHTML :: ReleasesMap -> Html ()
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
                td_ $ h3_ "extensions" >> arrows
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
        displayRelease (GHCRelease d (x,y,z)) = do
            h3_ $ toHtml $ "GHC-" <> intercalate "." (show <$> [x,y])
            arrows

        arrows = p_ ! class_ "arrows" $ do
                     span_ ! class_ "down-arrow" $ "ꜜ"
                     span_ ! class_ "up-arrow"   $ "ꜛ"

        script_' :: Text -> Html ()
        script_' x = makeElement "script" ! type_ "text/javascript" ! src_ x $ ""
        style_' hrefV = link_   [ type_ "text/css"
                                , rel_ "stylesheet"
                                , href_ hrefV ]
