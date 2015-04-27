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
    <|> route [ ("sounds",     sounds)
              , ("sounds/:id", sound )
              ]

sounds :: Snap ()
sounds =  method GET  indexSounds
      <|> method POST createSound

indexSounds :: Snap ()
indexSounds = undefined

createSound :: Snap ()
createSound = undefined

sound :: Snap ()
sound = method GET hearSound

hearSound :: Snap ()
hearSound = undefined
