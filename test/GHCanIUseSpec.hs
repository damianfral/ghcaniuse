{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHCanIUseSpec where

import Control.Concurrent.ParallelIO (parallelInterleaved)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (unpack)
import GHCanIUse.CLI
import GHCanIUse.Types
import Network.HTTP.Req
import Options.Generic (Unwrapped)
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
dir = "/nix/store/ar26ijy4qmgz8s6fnjfdcy9kyaf4r9ka-ghc-language-extensions"

options :: Options Unwrapped
options = Options dir False

versions :: [GHCVersion]
versions = [GHCVersion 7 10 3, GHCVersion 8 2 2, GHCVersion 9 2 2]

spec :: Spec
spec =
  describe "getGHCReleasesFor" $ forM_ versions $ \v -> do
    it ("produces valid URLS for version" <> show v) $ do
      releases <- liftIO $ getGHCReleasesFor (Set.fromList [v]) options
      case Set.toList releases of
        [release] -> do
          let docs = releaseLanguages release
          let urls = catMaybes $ Map.elems $ unLanguageExtensionsDocs docs
          void $ liftIO $ parallelInterleaved $ checkURLDocExists <$> urls
        [] -> expectationFailure $ "Couldn't process " <> show v
        _ -> expectationFailure "Produced mored releases than 1"
