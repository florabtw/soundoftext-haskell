{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Database
( createTables
, saveSound
) where

import Application (App(..))
import Snap.Snaplet (Handler)
import Snap.Snaplet.SqliteSimple (Only(..), Sqlite(..))
import Snap.Snaplet.SqliteSimple (execute)
import Control.Monad (unless)
import qualified Database.SQLite.Simple as S
import qualified Data.Text as T

data Sound = Sound { soundId   :: Int
                   , soundLang :: String
                   , soundText :: String
                   , soundPath :: String
                   } deriving (Show)

tableExists :: S.Connection -> String -> IO Bool
tableExists conn name = do
    r <- S.query conn "SELECT name FROM sqlite_master WHERE type='table' AND name=?" (Only name)
    case r of
        [Only (_ :: String)] -> return True
        _                    -> return False

createTables :: S.Connection -> IO ()
createTables conn = do
    schemaCreated <- tableExists conn "sounds"
    unless schemaCreated $
        S.execute_ conn
            (S.Query $
             T.concat [ "CREATE TABLE sounds ("
                      , "id INTEGER PRIMARY KEY, "
                      , "lang TEXT, "
                      , "text TEXT, "
                      , "path TEXT"
                      , ")"
                      ]
            )

saveSound :: String -> String -> String -> Handler App Sqlite ()
saveSound l t p =
    execute "INSERT INTO sounds (lang, text, path) VALUES (?,?,?)" (l, t, p)
