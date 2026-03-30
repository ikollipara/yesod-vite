{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Yesod.Vite
-- Description : Vite Integration for the Yesod Web Framework
-- Copyright   : (c) Ian Kollipara, 2026
-- License     : BSD-2-Clause
-- Maintainer  : ian.kollipara@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module provides an integration to @[vitejs](https://vite.dev)@, a modern frontend build tool,
-- to the "Yesod" web framework. This integration would allow the use of @vite@, setup in backend mode,
-- to work seamlessly with "Yesod".
--
-- = Quick Start
-- The assumption here is that your setup is similar to the default Yesod scaffold.
-- You will need to have installed both @yesod-static@ and @yesod-vite@ for this to work.
-- In addition, you will need the static subsite configured, as the route constructor is required.
-- - Run @npm init -y@ or the equivalent for your build tool.
--
-- - Run @npm i -D vite@ or the equivalent for your build tool.
--
-- - Create a @vite.config.js@ to include the following:
--
-- @
-- import { defineConfig } from "vite";
-- import * as path from "node:path";
--
-- export default defineConfig({
--   plugins: [],
--   base: "/static/",
--   build: {
--     outDir: path.resolve("./static"),
--     manifest: "manifest.json",
--     rollupOptions: {
--       input: path.resolve("./assets/src/app.js"),
--     },
--   },
-- });
-- @
--
-- - Install and configure "Yesod.Static" for your application.
--
-- - Setup 'YesodVite' as the following:
--
-- @
-- instance YesodVite App where
--   viteBuildDir :: app -> IO FilePath
--   vitebuildDir app = return $ appStaticDir app
--
--   viteInDev :: app -> IO Bool
--   viteinDev app = return . const True
--
--   viteRoute :: Route Static -> Route site
--   viteRoute = StaticR
-- @
--
-- - Add the @viteAsset \<asset-name\>@ to your default layout.
module Yesod.Vite
  ( YesodVite (..),
    decodeManifest,
    gatherAllCSS,
    gatherAllModules,
  )
where

import Data.Aeson
  ( FromJSON (parseJSON),
    eitherDecode,
    withObject,
    (.!=),
    (.:),
    (.:?),
  )
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Functor (($>))
import Data.List (nub)
import qualified Data.Map as M
import qualified Data.Text as T
import GHC.Generics (Generic)
import Yesod.Core
  ( MonadIO (liftIO),
    RenderRoute (Route),
    WidgetFor,
    Yesod,
    getYesod,
    whamlet,
  )
import Yesod.Static (Route (StaticRoute), Static)

-- * Types

-- | A manifest chunk as described by the vite configuration.
data ViteManifestChunk
  = ViteManifestChunk
  { src :: Maybe FilePath,
    file :: FilePath,
    css :: [FilePath],
    assets :: [FilePath],
    isEntry :: Bool,
    name :: Maybe T.Text,
    isDynamicEntry :: Bool,
    imports :: [T.Text],
    dynamicImports :: [T.Text]
  }
  deriving (Show, Generic)

instance FromJSON ViteManifestChunk where
  parseJSON = withObject "ViteManifestChunk" $ \o ->
    ViteManifestChunk
      <$> o .:? "src"
      <*> o .: "file"
      <*> o .:? "css" .!= mempty
      <*> o .:? "assets" .!= mempty
      <*> o .:? "isEntry" .!= False
      <*> o .:? "name"
      <*> o .:? "isDynamicEntry" .!= False
      <*> o .:? "imports" .!= mempty
      <*> o .:? "dynamicImports" .!= mempty

-- | A wrapper around a map to use for parsing the manifest file as described by vite.
type ViteManifest = M.Map T.Text ViteManifestChunk

-- | Attempt to decode a manifest
decodeManifest :: BS.ByteString -> Either String ViteManifest
decodeManifest = eitherDecode

-- | Recursively traverse the manifest, starting at the @assetName@ to find all unique css values.
gatherAllCSS ::
  -- | The entry in the manifest.json. This is specified by your @vite.config.js@ as your input
  T.Text ->
  -- | The actual manifest to traverse.
  ViteManifest ->
  -- | The paths to all the css.
  [FilePath]
gatherAllCSS
  entry
  man =
    let gatherAllCSS' :: T.Text -> [FilePath]
        gatherAllCSS' k' =
          case man M.!? k' of
            Nothing -> []
            Just e -> css e <> concat [gatherAllCSS' k | k <- imports e]
     in nub $ gatherAllCSS' entry

-- | Recursively traverse the manifest, starting at the @assetName@ to find all unique js modules.
gatherAllModules ::
  -- | The entry in the manifest.json. This is specified by your @vite.config.js@ as your input
  T.Text ->
  -- | The actual manifest to traverse.
  ViteManifest ->
  -- | The paths to all the js modules.
  [FilePath]
gatherAllModules entry man =
  let gatherAllModules' :: T.Text -> [FilePath]
      gatherAllModules' k' =
        case man M.!? k' of
          Nothing -> []
          Just e -> file e : concat [gatherAllModules' k | k <- imports e]
   in nub $ gatherAllModules' entry

-- | = YesodVite
-- This typeclass is used for configuring the uses of @vite@ in your "Yesod" application.
class (Yesod site) => YesodVite site where
  -- | The directory that vite builds to. This should match the output field in your @vite.config.js@.
  viteBuildDir :: site -> IO FilePath

  -- | Whether or not the site is in development. This is used to either serve production or development assets.
  viteInDev :: site -> IO Bool

  -- | How to create a static route. Typically you can set this to just `StaticR`.
  viteRoute :: Route Static -> Route site

  {-# MINIMAL viteBuildDir, viteInDev, viteRoute #-}

  -- | The url to serve development resources from.
  -- Defaults to @\http://127.0.0.1:5173\@
  viteDevUrl :: site -> IO String
  viteDevUrl = return . const "http://127.0.0.1:5173"

  -- | The path to the built manifest. Defaults to `buildDir` + @/.vite/manifest.json@
  viteManifest :: site -> IO FilePath
  viteManifest site_ = do
    basePath <- viteBuildDir site_
    return $ basePath <> "/.vite/manifest.json"

  -- | A helper used by @vite@ for @react@-based sites.
  -- This is entirely optional, unless you are using a @react@-based frontend site.
  -- This will be empty in production, but will enable hot reload in development.
  viteEnableReactRefresh :: WidgetFor site ()
  viteEnableReactRefresh = do
    site_ <- getYesod
    devUrl <- liftIO $ viteDevUrl site_
    inDev <- liftIO $ viteInDev site_
    if inDev
      then
        [whamlet|
        <script type="module">
          import RefreshRuntime from '#{devUrl}/@react-refresh';
          RefreshRuntime.injectIntoGlobalHook(window);
          window.$RefreshReg$ = () => {};
          window.$RefreshSig$ = () => (type) => type;
          window.__vite_plugin_react_preamble_installed__ = true;
      |]
      else
        return mempty

  -- | Insert a @vite@ asset into your site.
  -- In development this will point at the vite development server.
  -- In production, this will traverse the manifest and insert the correct tags
  -- to your static assets.
  viteAsset :: T.Text -> WidgetFor site ()
  viteAsset assetName = do
    site_ <- getYesod
    inDev <- liftIO $ viteInDev site_
    if inDev
      then viteDevAsset assetName site_
      else viteProductionAsset assetName site_

-- = Internal

-- | Handle the development asset
viteDevAsset :: (YesodVite site, Yesod site) => T.Text -> site -> WidgetFor site ()
viteDevAsset assetName s = do
  devUrl <- liftIO $ viteDevUrl s
  [whamlet|
          <script type="module" src="#{devUrl}/@vite/client">
          <script type="module" src="#{devUrl}/#{assetName}">
  |]

-- | Handle the production asset
viteProductionAsset :: (YesodVite site, Yesod site) => T.Text -> site -> WidgetFor site ()
viteProductionAsset assetName s = do
  manifestPath <- liftIO $ viteManifest s
  manifestContents <- liftIO $ BS.readFile manifestPath
  let manifestE = decodeManifest manifestContents
  case manifestE of
    Left err -> error err $> mempty
    Right manifest -> do
      let isJS = T.isSuffixOf ".js" assetName
          allCss = T.splitOn "/" . T.pack <$> gatherAllCSS assetName manifest
          allModules = T.splitOn "/" . T.pack <$> gatherAllModules assetName manifest
          assetUrl = T.splitOn "/" assetName
      [whamlet|
            $if isJS
              <script type="module" src="@{viteRoute $ StaticRoute assetUrl []}">
            $else
              <link ref="stylesheet" href="@{viteRoute $ StaticRoute assetUrl []}">
            $forall css <- allCss
              <link ref="stylesheet" href="@{viteRoute $ StaticRoute css []}">
            $forall module_ <- allModules
              <link ref="modulepreload" href="@{viteRoute $ StaticRoute module_ []}">
          |]
