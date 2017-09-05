{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lucid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Data.Monoid ((<>))

headMeta :: Html ()
headMeta = 
  do meta_ [charset_ "utf-8"]
     meta_ [httpEquiv_ "x-ua-compatible" , content_ "ie=edge"]
     meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]

stylesheets :: Html ()
stylesheets = do
    link_ [rel_ "stylesheet", href_ "/css/style.css"] 
    link_ [rel_ "stylesheet"
          , href_ ("https://fonts.googleapis.com/css?family=" 
                <> "Lato|Merriweather|"
                -- <> "Crimson+Text|Unica+One|"
                -- <> "Julius+Sans+One|Monda|"
                -- <> "Libre+Baskerville|Libre+Franklin|"
                -- <> "Arvo|Lato|"
                -- <> "Open+Sans|Work+Sans:300|"
                -- <> "Neuton|"
                -- <> "Alegreya|Questrial"
                -- <> "Open+Sans|Roboto+Condensed|"
                -- <> "Rubik|Trirong|"
                -- <> "Cormorant+Garamond|Proza+Libre|"
                -- <> "Istok+Web|Lora|"
                -- <> "Archivo+Black|Tenor+Sans|"
                -- <> "Roboto Mono|"
                -- <> "Cardo|Pathway+Gothic+One|"
                )]

mathjax :: Html ()
mathjax = do
  script_ [type_ "text/x-mathjax-config"] . toHtmlRaw $ T.concat
    ["MathJax.Hub.Config({"
    ,"  extensions: [\"tex2jax.js\"],"
    ,"  jax: [\"input/TeX\",\"output/HTML-CSS\"],"
    ,"  \"HTML-CSS\": {"
    ,"    styles: {\".MathJax_Preview\": {visibility: \"hidden\"}}"
    ,"  }"
    ,"});"]
  script_ [type_ "text/javascript", src_ "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-MML-AM_CHTML", async_ ""] ("" :: Text)

template :: Html ()
template = do
  doctype_ 
  html_ [lang_ "en"] $ do 
    head_ $ do
      headMeta
      stylesheets
      mathjax

    body_ $ do
      header_ $ do 
        h4_ $
          a_ [href_ "/"] "imagined saviors"
        nav_ navbarContents

      main_ [role_ "main"] $ do 
        h1_ "$title$"
        "$body$"

      footer_ $ do
        div_ "Built with Hakyll."

navbarContents :: Html ()
navbarContents =
  ul_ $ do
    li_ (a_ [href_ "/"]      "home")
    li_ (a_ [href_ "/about"] "about me")

main :: IO ()
main = T.putStrLn (renderText template)
