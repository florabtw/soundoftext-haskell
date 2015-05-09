{-# LANGUAGE OverloadedStrings #-}

module Site
( app
) where

import Snap.Snaplet (Handler, SnapletInit)
import Snap.Snaplet (makeSnaplet, nestSnaplet, addRoutes, snapletValue, withTop)
import Snap.Snaplet.Heist (heistInit, render, heistLocal)
import Snap.Snaplet.SqliteSimple (sqliteInit, sqliteConn)
import Heist ((##))
import Heist.Interpreted (Splice)
import Heist.Interpreted (runChildrenWithText, bindSplices)

import Application (heist, db)
import Application (App(..))
import Database (Sound(..))
import Database (createTables, getSoundById)
import SoundManager (soundsDir, findSound)
import Languages (lookupLanguage)

import Snap.Core (writeBS, method, getPostParam, finishWith, getParam)
import Snap.Core (modifyResponse, setResponseStatus, setHeader, getResponse, dir)
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

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

------------------------------------------------------------------------------
createSoundSuccessJSON :: Sound -> String
createSoundSuccessJSON sound =
    let sId          = fromIntegral $ soundId sound
        object       = [ ("success", JSBool True)
                       , ("id",      JSRational False sId)
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
    modifyResponse $ setHeader "Content-Type" "application/json"
    modifyResponse $ setResponseStatus code str
    writeBS . fromString . errorJSON $ toString str
    getResponse >>= finishWith

toString :: B.ByteString -> String
toString = T.unpack . E.decodeUtf8

------------------------------------------------------------------------------
soundSplice :: Monad m => Sound -> Splice m
soundSplice (Sound _ lang text path) =
    runChildrenWithText splices
      where
        splices = do
          "lang" ## T.pack (lookupLanguage lang)
          "text" ## T.pack text
          "path" ## T.pack ('/' : soundsDir ++ path)

------------------------------------------------------------------------------
handleSounds :: Handler App App ()
handleSounds =  method GET  indexSounds
            <|> method POST createSound

indexSounds :: Handler App App ()
indexSounds = undefined

createSound :: Handler App App ()
createSound = do
    modifyResponse $ setHeader "Content-Type" "application/json"
    lang <- getPostParam "lang"
    text <- getPostParam "text"
    when (isNothing lang) $ finishEarly 400 "Parameter 'lang' missing!"
    when (isNothing text) $ finishEarly 400 "Parameter 'text' missing!"
    sound <- findSound (fromJust lang) (fromJust text)
    when (isNothing sound) $ finishEarly 400 "Unable to retrieve sound!"
    writeBS . fromString . createSoundSuccessJSON $ fromJust sound

handleSound :: Handler App App ()
handleSound = method GET showSound

showSound :: Handler App App ()
showSound = do
    sId <- getParam "id"
    when (isNothing sId) $ finishEarly 400 "Sound id is incorrect!"
    sound <- withTop db (getSoundById . read . toString $ fromJust sId)
    when (null sound) $ finishEarly 400 "No sound found with that id!"
    let splices = bindSplices $ "sound" ## soundSplice (head sound)
    heistLocal splices $ render "sound"

serveStatic :: Handler App App ()
serveStatic =  dir "sounds"      (serveDirectory soundsDir)
           <|> dir "stylesheets" (serveDirectory "static/stylesheets")
           <|> dir "js"          (serveDirectory "static/js")

routes :: [(B.ByteString, Handler App App ())]
routes = [ ("/",           render "index")
         , ("/sounds",     handleSounds)
         , ("/sounds/:id", handleSound)
         , ("/static",     serveStatic)
         ]

app :: SnapletInit App App
app = makeSnaplet "app" "Sound of Text" Nothing $ do
    addRoutes routes
    h <- nestSnaplet "" heist $ heistInit "templates"
    d <- nestSnaplet "db" db sqliteInit

    let c = sqliteConn $ d ^# snapletValue
    liftIO $ withMVar c $ \conn -> createTables conn

    return $ App h d
