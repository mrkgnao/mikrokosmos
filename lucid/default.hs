{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lucid
import qualified Data.Text.Lazy.IO as T

headMeta :: Html ()
headMeta = 
  do meta_ [charset_ "utf-8"]
     meta_ [httpEquiv_ "x-ua-compatible" , content_ "ie=edge"]
     meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]

stylesheets :: Html ()
stylesheets =
    link_ [rel_ "stylesheet", href_ "/css/main.css"] 

template :: Html ()
template = do
  doctype_ 
  html_ [lang_ "en"] $ do 
    head_ $ do
      headMeta
      stylesheets

    body_ $ do
      header_ $ do 
        h4_ $
          a_ [href_ "/"] "imagined saviors"
        nav_ $ 
          ul_ $ do
            li_ (a_ [href_ "/"]      "home")
            li_ (a_ [href_ "/about"] "about me")

      main_ [role_ "main"] $ do 
        h1_ "$title$"
        "$body$"

      footer_ $ do
        div_ "hakyll/lucid"

main :: IO ()
main = T.putStrLn (renderText template)
