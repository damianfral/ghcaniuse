{-# LANGUAGE FlexibleContexts #-}

module GHCanIUse.Render where

import           BasicPrelude      hiding (lookup)
import           Data.HashMap.Lazy hiding (filter, (!))
import qualified Data.HashMap.Lazy as M
import           Data.Text         (pack, unpack)
import           GHCanIUse.Types
import           GHCanIUse.Utils
import           Lucid
import           Lucid.Base

(!) a b = a `with` [b]

displayRelease :: GHCRelease -> Html ()
displayRelease (GHCRelease d (x,y,z)) = do
    h3_ $ toHtml $ "GHC-" <> intercalate "." (show <$> [x,y])
    arrows

arrows = p_ ! class_ "arrows" $ do
             span_ ! class_ "down-arrow" $ "ꜜ"
             span_ ! class_ "up-arrow"   $ "ꜛ"

generateHTMLTable :: ReleasesMap -> DocLinksMap -> Html ()
generateHTMLTable releasesMap docLinksMap = table_ ! id_ "ghc-extensions" $ do
    thead_ $ tr_ $ do
        td_ $ h3_ "extensions" >> arrows
        mapM_ td_ $ displayRelease <$> allReleases
    tbody_ $
        forM_ allExtensions $ \extension ->
            tr_ $ do
                td_ $ toHtml extension
                forM_ allReleases $ \release ->
                    go extension release $ elem release <$> lookup extension releasesMap
    where
        allExtensions  = sort $ filterExtensions $ keys releasesMap
        allReleases    = reverse $ sort $ nub $ mconcat $ snd <$> toList releasesMap
        latestRelease = maximum allReleases

        go ext release (Just True) = td_ ! class_ "supported" $
            a_ ! href_ (getDocLink' ext release) $ "✓"
        go _ _ _ = td_ ! class_ "not-supported" $ p_ "-"

        getDocLink' = getDocLink releasesMap docLinksMap

getDocLink :: ReleasesMap -> DocLinksMap -> Text -> GHCRelease -> Text
getDocLink releasesMap docLinksMap extension release = pack . mconcat $ catMaybes
    [ Just $ ghcUserGuideURL release
    , lookup (extension, release) docLinksMap ]



generatePage :: Html () -> Html ()
generatePage content = html_ $ do
    head_ $ do
        title_   $ "GHCanIUse"
        style_' "https://fonts.googleapis.com/css?family=Roboto"
        style_' "style.css"
        script_ [type_ "text/javascript"]
            ("(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)})(window,document,'script','//www.google-analytics.com/analytics.js','ga');ga('create', 'UA-42041306-4', 'auto');ga('send', 'pageview');"::Text)

    body_ $ do
        content
        script_' "tablesort.min.js"
        script_ [type_ "text/javascript"]
            ("new Tablesort(document.getElementById('ghc-extensions'));"::Text)

    where
        script_' :: Text -> Html ()
        script_' x = makeElement "script" ! type_ "text/javascript" ! src_ x $ ""
        style_' hrefV = link_   [ type_ "text/css"
                                , rel_ "stylesheet"
                                , href_ hrefV ]

generateIndexPage r d = generatePage $ generateHTMLTable r d

-- generateExtensionPage :: Text -> ReleasesMap -> DocLinksMap -> Html ()
-- generateExtensionPage ext releasesMap docLinksMap = generatePage $ do
--     h3_ $ toHtml ext
--     dl_ $ do
--         dt_ $ toHtml ext
--         dd_ $ toHtml $ fromMaybe "No description avaliable." $ doc
--         generateHTMLTable $ filterWithKey (\k _ -> k == ext) releasesMap
--     where
--         doc = lookup ext releasesMap >>= \r -> lookup (ext, maximum r) docLinksMap
--         -- docLinksMap (ext, maximum $ releasesMap M.! ext)
