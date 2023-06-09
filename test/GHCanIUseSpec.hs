{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHCanIUseSpec where

import Control.Concurrent.Async (forConcurrently_)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (unpack)
import GHCanIUse
import GHCanIUse.Types
import Network.HTTP.Req
import Relude
import Test.Syd
import Text.URI

checkURLDocExists :: URI -> IO ()
checkURLDocExists url' = do
  case useHttpsURI url' of
    Nothing -> expectationFailure $ unpack $ "Not valid url: " <> render url'
    Just (url, _) -> do
      putStrLn $ unpack $ render url'
      let r = req GET url NoReqBody ignoreResponse mempty
      response <- runReq defaultHttpConfig r
      responseStatusCode response `shouldBe` 200

dir :: FilePath
dir = "/nix/store/mdl1y1jfmjrvflccks4j30xb4xdrir1w-ghc-language-extensions"

versions :: [GHCVersion]
versions = [GHCVersion 7 10 3, GHCVersion 8 2 2, GHCVersion 9 2 2]

spec :: Spec
spec =
  describe "getGHCReleasesFor" $ forM_ versions $ \v -> do
    it ("produces valid URLS for version" <> show v) $ do
      releases <- liftIO $ getGHCReleasesFor (Set.fromList [v]) dir
      case Set.toList releases of
        [release] -> do
          let docs = releaseLanguages release
          let urls = catMaybes $ Map.elems $ unLanguageExtensionsDocs docs
          forConcurrently_ urls checkURLDocExists
        [] -> expectationFailure $ "Couldn't process " <> show v
        _ -> expectationFailure "Produced mored releases than 1"
