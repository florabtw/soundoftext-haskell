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
import Heist.Interpreted (runChildrenWithText, bindSplices, mapSplices)

import Application (heist, db)
import Application (App(..))
import Database (Sound(..))
import Database (createTables, getSoundById)
import SoundManager (soundsDir, findSound)
import Languages (lookupLanguage, languagePairs)

import Snap.Core (writeBS, method, getPostParam, finishWith, getParam, dir)
import Snap.Core (modifyResponse, setResponseStatus, setHeader, getResponse)
import Snap.Core (rqPathInfo, getRequest)
import Snap.Core (Method(..), MonadSnap)
import Snap.Util.FileServe (serveDirectory, serveFile)

import Control.Applicative ((<|>))
import Control.Concurrent (withMVar)
import Control.Lens ((^#))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import Data.List (sortBy)
import Data.Maybe (isNothing, fromJust)
import Data.Ord (comparing)
import Data.String (fromString)
import Network.HTTP.Base (urlDecode, urlEncode)
import System.FilePath ((</>), takeFileName, dropFileName)
import Text.JSON (makeObj, encode)
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

finishEarly :: (MonadSnap m) => Int -> B.ByteString -> m b
finishEarly code str = do
    modifyResponse $ setHeader "Content-Type" "text/plain"
    modifyResponse $ setResponseStatus code str
    writeBS str
    getResponse >>= finishWith

toString :: B.ByteString -> String
toString = T.unpack . E.decodeUtf8

toBS :: String -> B.ByteString
toBS = E.encodeUtf8 . T.pack

-- Ensure all text input is:
--   100 characters or fewer (for Google Translate)
--   lowercase
standardizeText :: String -> String
standardizeText = map toLower . take 100

------------------------------------------------------------------------------
soundSplice :: Monad m => Sound -> Splice m
soundSplice (Sound _ lang text path) =
    runChildrenWithText splices
      where
        encodedPath = (dropFileName path) </> (urlEncode $ takeFileName path)
        splices     = do
          "lang" ## T.pack (lookupLanguage lang)
          "text" ## T.pack text
          "path" ## T.pack ('/' : soundsDir </> encodedPath)

langSplice :: Monad m => (String, String) -> Splice m
langSplice (key, name) =
    runChildrenWithText splices
    where
        splices = do
            "key"  ## T.pack key
            "name" ## T.pack name

------------------------------------------------------------------------------
handleIndex :: Handler App App ()
handleIndex = do
    let sortedPairs = sortBy (comparing snd) languagePairs
        splices = bindSplices $ "languages" ## mapSplices langSplice sortedPairs
    heistLocal splices $ render "soundoftext"

handleSounds :: Handler App App ()
handleSounds =  method GET  indexSounds
            <|> method POST createSound

indexSounds :: Handler App App ()
indexSounds = undefined

createSound :: Handler App App ()
createSound = do
    mLang <- getPostParam "lang"
    mText <- getPostParam "text"
    when (isNothing mLang) $ finishEarly 400 "Parameter 'lang' missing!"
    when (isNothing mText) $ finishEarly 400 "Parameter 'text' missing!"
    let lang = toString $ fromJust mLang
        text = standardizeText . toString $ fromJust mText
    sound <- findSound lang text
    when (isNothing sound) $ finishEarly 400 "Unable to retrieve sound!"
    modifyResponse $ setHeader "Content-Type" "application/json"
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
serveStatic =  dir "sounds"      serveSound
           <|> dir "stylesheets" (serveDirectory "static/stylesheets")
           <|> dir "js"          (serveDirectory "static/js")

serveSound :: Handler App App ()
serveSound = do
    request <- getRequest
    let relPath     = urlDecode . toString $ rqPathInfo request
        disposition = toBS $ "attachment; filename=" ++ takeFileName relPath
    modifyResponse $ setHeader "Content-Disposition" disposition
    serveFile $ soundsDir </> relPath

notFound :: Handler App App ()
notFound = finishEarly 404 "Page cannot be found!"

routes :: [(B.ByteString, Handler App App ())]
routes = [ ("/",           handleIndex)
         , ("/sounds",     handleSounds)
         , ("/sounds/:id", handleSound)
         , ("/results",    render "results")
         , ("/static",     serveStatic)
         , ("/:any",       notFound)
         ]

app :: SnapletInit App App
app = makeSnaplet "app" "Sound of Text" Nothing $ do
    addRoutes routes
    h <- nestSnaplet "" heist $ heistInit "templates"
    d <- nestSnaplet "db" db sqliteInit

    let c = sqliteConn $ d ^# snapletValue
    liftIO $ withMVar c $ \conn -> createTables conn

    return $ App h d
