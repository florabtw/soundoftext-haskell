{-# LANGUAGE OverloadedStrings #-}
module Main where

import Snap.Core (ifTop, writeText, writeBS, route, method, getPostParam, finishWith)
import Snap.Core (modifyResponse, setResponseStatus, addHeader, getResponse)
import Snap.Core (Snap, Method(..), MonadSnap)
import Snap.Http.Server (quickHttpServe)
import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isNothing, fromJust)
import SoundManager (getSoundPath)
import Data.ByteString as B

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =  ifTop (writeText "Coming soon!")
    <|> route [ ("sounds",     sounds)
              , ("sounds/:id", sound )
              ]

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
    filePath <- liftIO $ getSoundPath (fromJust lang) (fromJust text)
    when (isNothing filePath) $ finishEarly 400 "Unable to retrieve sound!"
    writeBS $ fromJust filePath


sound :: Snap ()
sound = method GET hearSound

hearSound :: Snap ()
hearSound = undefined

finishEarly :: (MonadSnap m) => Int -> ByteString -> m b
finishEarly code str = do
    modifyResponse $ setResponseStatus code str
    modifyResponse $ addHeader "Content-Type" "text/plain"
    writeBS str
    getResponse >>= finishWith
