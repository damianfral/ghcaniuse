{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHCanIUse.Render where

import Data.FileEmbed
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Version (showVersion)
import GHCanIUse.Types
import Lucid
import qualified Paths_ghcaniuse as GHCanIUse
import Relude
import Text.URI (render)

(!) :: (With a) => a -> Attribute -> a
(!) a b = a `with` [b]

flexRow :: (With (arg -> result), Term arg result) => arg -> result
flexRow = div_ ! class_ "flex justify-center"

displayReleaseHTML :: GHCRelease -> Html ()
displayReleaseHTML release = flexRow $ h4_ [class_ "ttl"] link >> arrows
  where
    href = render $ releaseWeb release
    link = a_ [href_ href, class_ "black"] $ toHtml $ displayRelease release

arrows :: Html ()
arrows = p_ [class_ "arrows tc f3 pl2"] do
  span_ ! class_ "down-arrow ml-1" $ "↓"
  span_ ! class_ "up-arrow ml-1" $ "↑"

generateHTMLTable :: LanguageExtensionsMap -> Html ()
generateHTMLTable languageExtensions = article_
  ! class_ "w100"
  $ table_ [id_ "ghc-extensions-table", class_ "bg-white m0"] do
    thead_ $ tr_ [class_ "bg-white"] do
      th_ $ flexRow $ h4_ "Language Extensions" >> arrows
      mapM_ th_ $ displayReleaseHTML <$> allReleases
    tbody_
      ! class_ "lh-solid"
      $ forM_ allExtensions
      $ \extension -> tr_ do
        th_ ! class_ "tl bg-white" $ toHtml $ unLanguageExtension extension
        forM_ allReleases $ renderCell extension
  where
    allExtensions = Set.toList $ filterExtensions $ M.keysSet languageExtensions
    allReleases = reverse $ Set.toList $ fold $ M.elems languageExtensions

renderCell :: LanguageExtension -> GHCRelease -> Html ()
renderCell ext release =
  td_
    [ class_ classNames,
      title_ title,
      data_ "sort" (mapExtensionSupport "1" "2" $ const "3")
    ]
    cellContent
  where
    title = displayLangAtVersion ext $ releaseVersion release
    extSupported = supportsExtension ext release
    extDoc = getExtensionnLanguageDoc ext release
    mapExtensionSupport notSupported supportedNoDoc supported =
      if extSupported
        then maybe supportedNoDoc supported extDoc
        else notSupported
    classNames =
      "tc ba b--white "
        <> mapExtensionSupport
          "not-supported"
          "supported-no-doc"
          (const "supported")
    cellContent = mapExtensionSupport "no" "yes" $ \url ->
      a_ [href_ (render url), class_ "white"] "yes"

template :: Html () -> Html ()
template content = doctypehtml_ do
  head_ do
    title_ "GHC Language Extensions Compatibility"
    traverse_ meta_ metas
    style_' "https://unpkg.com/tachyons@4.12.0/css/tachyons.css"
    style_ stylesheet
    script_
      [ defer_ "true",
        type_ "text/javascript",
        src_ "https://www.googletagmanager.com/gtag/js?id=G-M6ML66CBGJ"
      ]
      ("" :: Text)
    script_ [type_ "text/javascript"] googleTag

  body_ [class_ "f4 bg-black pr flex flex-column"] do
    header
    content
    script_ [type_ "text/javascript"]
      $ tableSort
      <> "\nnew Tablesort(document.getElementById('ghc-extensions-table'));"
  where
    keywords = "GHC, haskell, language extensions, table, nix, NixOS, table"
    description = "Browse the supported language extensions for different GHC versions."
    metas =
      [ [charset_ "UTF-8"],
        [name_ "viewport", content_ "width=device-width, initial-scale=1"],
        [name_ "author", content_ "damianfral"],
        [name_ "keywords", content_ keywords],
        [name_ "description", content_ description]
      ]
    style_' hrefV = link_ [type_ "text/css", rel_ "stylesheet", href_ hrefV]
    header = header_
      [class_ "bg-white flex flex-row justify-stretch items-center"]
      $ do
        a_
          ! title_ "ghcaniuse github repo"
          ! href_ "https://github.com/damianfral/ghcaniuse"
          $ div_ [class_ "flex items-center pa4 bg-white black"] do
            img_ [src_ githubSVG, class_ "mr2 bg-white", alt_ "github logo"]
            p_ ! class_ "ma0" $ "damianfral/ghcaniuse"
        p_ ! class_ "mr4 tr flex-auto white" $ do
          toHtml $ "v" <> showVersion GHCanIUse.version

generatePage :: LanguageExtensionsMap -> Html ()
generatePage = template . generateHTMLTable

fromUtf8BS :: ByteString -> Text
fromUtf8BS = decodeUtf8With lenientDecode

stylesheet :: Text
stylesheet =
  fromUtf8BS $(embedFile =<< makeRelativeToProject "./assets/style.css")

tableSort :: Text
tableSort =
  fromUtf8BS $(embedFile =<< makeRelativeToProject "./assets/tablesort.min.js")

googleTag :: Text
googleTag =
  fromUtf8BS $(embedFile =<< makeRelativeToProject "./assets/google-tag.js")

githubSVG :: Text
githubSVG =
  fromUtf8BS $(embedFile =<< makeRelativeToProject "./assets/github.svg")
