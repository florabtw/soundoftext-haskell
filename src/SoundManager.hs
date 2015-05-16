module SoundManager
( soundsDir
, findSound
) where

import Database (Sound(..))
import Database (saveSound, getSoundByLangTextPair)
import Application (App(..))
import Application (db)

import Snap.Snaplet (Handler, withTop)

import Network.HTTP.Base (urlEncode)
import Network.HTTP.Client (httpLbs, newManager, defaultManagerSettings)
import Network.HTTP.Client (parseUrl, responseBody)
import Network.HTTP.Client (Manager)
import System.FilePath ((</>), (<.>))
import System.Directory (createDirectoryIfMissing)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as L

soundsDir :: String
soundsDir = "static/sounds"

soundsExt :: String
soundsExt = "mp3"

rootUrl :: String
rootUrl = "http://translate.google.com/translate_tts?ie=UTF-8"

cManager :: IO Manager
cManager = newManager defaultManagerSettings

findSound :: String -> String-> Handler App App (Maybe Sound)
findSound lang text = do
    sounds <- withTop db $ getSoundByLangTextPair lang text
    if null sounds
        then createSound lang text
        else return . Just $ head sounds

createSound :: String -> String -> Handler App App (Maybe Sound)
createSound lang text = do
    path <- liftIO $ downloadSound lang text
    withTop db $ saveSound lang text path
    sounds <- withTop db $ getSoundByLangTextPair lang text
    if null sounds
        then return Nothing
        else return . Just $ head sounds

downloadSound :: String -> String -> IO String
downloadSound lang text = do
    let encodedText = urlEncode text
        url         = addTextParam encodedText . addLangParam lang $ rootUrl
    req <- parseUrl url
    man <- cManager
    res <- httpLbs req man
    let pathText = map toFilePath text
        absPath  = makeAbsSoundPath lang pathText
        relPath  = makeRelSoundPath lang pathText
    createDirectoryIfMissing True $ soundsDir </> lang
    L.writeFile absPath $ responseBody res
    return relPath

toFilePath :: Char -> Char
toFilePath ' ' = '_'
toFilePath '/' = '-'
toFilePath c   = c

makeAbsSoundPath :: String -> String -> String
makeAbsSoundPath lang text = soundsDir </> makeRelSoundPath lang text

makeRelSoundPath :: String -> String -> String
makeRelSoundPath lang text = lang </> text <.> soundsExt

addParam :: String -> String -> String -> String
addParam k v s = s ++ "&" ++ k ++ "=" ++ v

addLangParam :: String -> String -> String
addLangParam = addParam "tl"

addTextParam :: String -> String -> String
addTextParam = addParam "q"
