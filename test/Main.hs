{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import qualified Data.ByteString.Lazy as BS
import Data.Either (either, isRight)
import Debug.Trace
import Test.Hspec
import TestApp
import Yesod.Core
import Yesod.Static
import Yesod.Test
import Yesod.Vite

mkYesod
  "App"
  [parseRoutes|
  /static StaticR Static appStatic
  /react RrR GET
  / HomeR GET
|]

getRrR :: Handler Html
getRrR = defaultLayout viteEnableReactRefresh

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  [whamlet|
      ^{viteAsset "views/foo.js"}
      <main>Hello World
    |]

instance Yesod App

instance YesodVite App where
  viteBuildDir :: App -> IO FilePath
  viteBuildDir _ = return "static"

  viteRoute = StaticR

  viteInDev :: App -> IO Bool
  viteInDev = return . appInDev

  viteManifest :: App -> IO FilePath
  viteManifest s =
    if appInDev s
      then do
        basePath <- viteBuildDir s
        return $ basePath <> "/.vite/manifest.json"
      else
        return "test/exampleManifest.json"

manifestSpec :: Spec
manifestSpec = do
  describe "Manifest Parsing" $ do
    it "should parse the manifest" $ do
      manifest <- liftIO $ BS.readFile "test/exampleManifest.json"
      decodeManifest manifest `shouldSatisfy` isRight

    it "should extract all the CSS" $ do
      manifest <- liftIO $ BS.readFile "test/exampleManifest.json"
      let em = decodeManifest manifest
      either
        fail
        ( \m ->
            let css_ = gatherAllCSS "views/foo.js" m
             in css_ `shouldNotSatisfy` null
        )
        em

    it "should gather all the modules" $ do
      manifest <- liftIO $ BS.readFile "test/exampleManifest.json"
      let em = decodeManifest manifest
      either
        fail
        ( \m ->
            let mods_ = gatherAllModules "views/foo.js" m
             in mods_ `shouldNotSatisfy` null
        )
        em

yesodViteSpec :: Spec
yesodViteSpec = do
  describe "YesodVite" $ do
    it "should return the expected path" $ do
      app <- liftIO $ getApp True
      manifest <- liftIO $ viteManifest app
      manifest `shouldBe` ("static/.vite/manifest.json" :: FilePath)

yesodViteWithYesodSpecDev :: Spec
yesodViteWithYesodSpecDev =
  yesodSpecWithSiteGenerator (getApp True) $ do
    ydescribe "YesodVite (Widgets) [DEV]" $ do
      yit "should get a correct response for RrR" $ do
        get RrR
        statusIs 200
        bodyContains "@react-refresh"

      yit "should have development vite urls" $ do
        get HomeR
        statusIs 200
        bodyContains "@vite"

yesodViteWithYesodSpecProd :: Spec
yesodViteWithYesodSpecProd =
  yesodSpecWithSiteGenerator (getApp False) $ do
    ydescribe "YesodVite (Widgets) [PROD]" $ do
      yit "should contain an empty response" $ do
        get RrR
        statusIs 200
        bodyNotContains "@react-refresh"
      yit "should return the full vite assets" $ do
        get HomeR
        statusIs 200
        bodyNotContains "@vite"

main :: IO ()
main =
  hspec $
    manifestSpec
      *> yesodViteSpec
      *> yesodViteWithYesodSpecDev
      *> yesodViteWithYesodSpecProd
