module SoundManager
( getSoundPath
, soundsDir
) where

import Network.HTTP.Client
import System.FilePath ((</>), (<.>))
import System.Directory (doesFileExist, createDirectoryIfMissing)
import Data.Maybe (isNothing)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

soundsDir :: String
soundsDir = "/home/soundoftext/sounds"

soundsExt :: String
soundsExt = "mp3"

rootUrl :: String
rootUrl = "http://translate.google.com/translate_tts?ie=UTF-8"

cManager :: IO Manager
cManager = newManager defaultManagerSettings

getSoundPath :: B.ByteString -> B.ByteString -> IO (Maybe B.ByteString)
getSoundPath lang text = do
    filePath <- locateSound lang text
    if isNothing filePath
        then downloadSound lang text
        else return filePath

locateSound :: B.ByteString -> B.ByteString -> IO (Maybe B.ByteString)
locateSound lang text = do
    let langString = toString lang
        textString = toString text
        absPath    = makeAbsSoundPath langString textString
        relPath    = makeRelSoundPath langString textString
    fileExists  <- doesFileExist absPath
    if fileExists
      then return . Just $ toBS relPath
      else return Nothing

downloadSound :: B.ByteString -> B.ByteString -> IO (Maybe B.ByteString)
downloadSound lang text = do
    let langString = toString lang
        textString = toString text
        url        = addTextParam textString . addLangParam langString $ rootUrl
    req <- parseUrl url
    man <- cManager
    res <- httpLbs req man
    let absPath = makeAbsSoundPath langString textString
        relPath = makeRelSoundPath langString textString
    createDirectoryIfMissing True $ soundsDir </> langString
    L.writeFile absPath $ responseBody res
    return . Just $ toBS relPath

makeAbsSoundPath :: String -> String -> String
makeAbsSoundPath lang text = soundsDir </> makeRelSoundPath lang text

makeRelSoundPath :: String -> String -> String
makeRelSoundPath lang text = lang </> text <.> soundsExt

toString :: B.ByteString -> String
toString = T.unpack . E.decodeUtf8

toBS :: String -> B.ByteString
toBS = E.encodeUtf8 . T.pack

addParam :: String -> String -> String -> String
addParam k v s = s ++ "&" ++ k ++ "=" ++ v

addLangParam :: String -> String -> String
addLangParam = addParam "tl"

addTextParam :: String -> String -> String
addTextParam = addParam "q"
