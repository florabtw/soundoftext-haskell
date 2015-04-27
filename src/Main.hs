{-# LANGUAGE OverloadedStrings #-}
module Main where

import Snap.Core (ifTop, writeText, route, method)
import Snap.Core (Snap, Method(..))
import Snap.Http.Server (quickHttpServe)
import Control.Applicative ((<|>))

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =  ifTop (writeText "Coming soon!")
    <|> route [ ("sounds", sounds) ]

sounds :: Snap ()
sounds =  method GET  hearSound
      <|> method POST createSound

hearSound :: Snap ()
hearSound = undefined

createSound :: Snap ()
createSound = undefined
