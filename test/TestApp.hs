module TestApp where
import Yesod.Static (Static, static)


data App = App {
  appStatic :: Static,
  appInDev :: Bool
}

getApp :: Bool -> IO App
getApp indev = do
  s <- static "./"
  return $ App s indev
