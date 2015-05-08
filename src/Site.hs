{-# LANGUAGE OverloadedStrings #-}

module Site
( app
) where

import Snap.Snaplet (Handler, SnapletInit)
import Snap.Snaplet (makeSnaplet, nestSnaplet, addRoutes, snapletValue, withTop)
import Snap.Snaplet.Heist (heistInit, render)
import Snap.Snaplet.SqliteSimple (sqliteInit, sqliteConn)

import Application (heist, db)
import Application (App(..))

import Database (createTables, saveSound)

import Snap.Core (writeBS, method, getPostParam, finishWith)
import Snap.Core (modifyResponse, setResponseStatus, addHeader, getResponse, dir)
import Snap.Core (Method(..), MonadSnap)
import Snap.Util.FileServe (serveDirectory)

import Control.Applicative ((<|>))
import Control.Concurrent (withMVar)
import Control.Lens ((^#))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isNothing, fromJust)
import Data.String (fromString)
import Text.JSON (toJSString, makeObj, encode)
import Text.JSON (JSValue(..))
import SoundManager (getSoundPath, soundsDir)

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

------------------------------------------------------------------------------
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

finishEarly :: (MonadSnap m) => Int -> B.ByteString -> m b
finishEarly code str = do
    modifyResponse $ setResponseStatus code str
    modifyResponse $ addHeader "Content-Type" "text/plain"
    writeBS . fromString . errorJSON $ toString str
    getResponse >>= finishWith

toString :: B.ByteString -> String
toString = T.unpack . E.decodeUtf8

------------------------------------------------------------------------------
handleSounds :: Handler App App ()
handleSounds =  method GET  indexSounds
            <|> method POST createSound

indexSounds :: Handler App App ()
indexSounds = undefined

createSound :: Handler App App ()
createSound = do
    lang <- getPostParam "lang"
    text <- getPostParam "text"
    when (isNothing lang) $ finishEarly 400 "Parameter 'lang' missing!"
    when (isNothing text) $ finishEarly 400 "Parameter 'text' missing!"
    path <- liftIO $ getSoundPath (fromJust lang) (fromJust text)
    when (isNothing path) $ finishEarly 400 "Unable to retrieve sound!"
    withTop db $ saveSound (toString $ fromJust lang) (toString $ fromJust text) (toString $ fromJust path)
    writeBS . fromString . createSoundSuccessJSON . toString $ fromJust path

serveStatic :: Handler App App ()
serveStatic =  dir "sounds"      (serveDirectory soundsDir)
           <|> dir "stylesheets" (serveDirectory "static/stylesheets")

routes :: [(B.ByteString, Handler App App ())]
routes = [ ("/",       render "index")
         , ("/sounds", handleSounds)
         , ("/static", serveStatic)
         ]

app :: SnapletInit App App
app = makeSnaplet "app" "Sound of Text" Nothing $ do
    addRoutes routes
    h <- nestSnaplet "" heist $ heistInit "templates"
    d <- nestSnaplet "db" db sqliteInit

    let c = sqliteConn $ d ^# snapletValue
    liftIO $ withMVar c $ \conn -> createTables conn

    return $ App h d
