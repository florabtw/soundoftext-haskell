{-# LANGUAGE OverloadedStrings #-}
module Main where

import Snap.Core (ifTop, writeBS, route, method, getPostParam, finishWith)
import Snap.Core (modifyResponse, setResponseStatus, addHeader, getResponse, dir)
import Snap.Core (Snap, Method(..), MonadSnap)
import Snap.Util.FileServe (serveDirectory, serveFile)
import Snap.Http.Server (quickHttpServe)
import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isNothing, fromJust)
import Data.ByteString as B
import Data.String (fromString)
import Text.JSON (toJSString, makeObj, encode)
import Text.JSON (JSValue(..))
import SoundManager (getSoundPath, soundsDir)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =  ifTop (serveFile "templates/index.html")
    <|> route [ ("sounds", sounds) ]
    <|> dir "static" serveStatic

sounds :: Snap ()
sounds =  method GET  indexSounds
      <|> method POST createSound

indexSounds :: Snap ()
indexSounds = undefined

createSound :: Snap ()
createSound = do
    lang <- getPostParam "lang"
    text <- getPostParam "text"
    when (isNothing lang) $ finishEarly 400 "Parameter 'lang' missing!"
    when (isNothing text) $ finishEarly 400 "Parameter 'text' missing!"
    path <- liftIO $ getSoundPath (fromJust lang) (fromJust text)
    when (isNothing path) $ finishEarly 400 "Unable to retrieve sound!"
    writeBS . fromString . createSoundSuccessJSON . toString $ fromJust path

serveStatic :: Snap ()
serveStatic = dir "sounds" (serveDirectory soundsDir)

-- helper methods

createSoundSuccessJSON :: String -> String
createSoundSuccessJSON path =
    let pathJSString = toJSString path
        object       = [ ("success", JSBool True)
                       , ("path",    JSString pathJSString)
                       ]
    in  encode $ makeObj object

errorJSON :: String -> String
errorJSON m =
    let messageJSString = toJSString m
        object          = [ ("success", JSBool False)
                          , ("message", JSString messageJSString)
                          ]
    in encode $ makeObj object

finishEarly :: (MonadSnap m) => Int -> ByteString -> m b
finishEarly code str = do
    modifyResponse $ setResponseStatus code str
    modifyResponse $ addHeader "Content-Type" "text/plain"
    writeBS . fromString . errorJSON $ toString str
    getResponse >>= finishWith

toString :: ByteString -> String
toString = T.unpack . E.decodeUtf8
