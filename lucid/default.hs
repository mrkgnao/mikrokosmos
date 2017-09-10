{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lucid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text as Strict
import qualified Data.Text.Lazy.IO as T
import Data.Monoid ((<>))

headMeta :: Html ()
headMeta = 
  mapM_ meta_ 
    [ [charset_ "utf-8"]
    , [httpEquiv_ "x-ua-compatible" , content_ "ie=edge"]
    , [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    ]

stylesheet_ :: Monad m => Strict.Text -> HtmlT m ()
stylesheet_ lincc = link_ [rel_ "stylesheet", href_ lincc]

stylesheets :: Html ()
stylesheets = 
  mapM_ stylesheet_ 
    [ "/css/reset.css"
    , "/css/style.css"
    , googleFontsLink
    ]
  where googleFontsLink = 
            "https://fonts.googleapis.com/css?family=" 
         <> "Lato|Merriweather"
         <> "Crimson+Text|Unica+One|"
         <> "Julius+Sans+One|Monda|"
         <> "Libre+Baskerville|Libre+Franklin|"
         <> "Arvo|Lato|"
         <> "Open+Sans|Work+Sans:300|"
         <> "Neuton|"
         <> "Alegreya|Questrial"
         <> "Open+Sans|Roboto+Condensed|"
         <> "Rubik|Trirong|"
         <> "Cormorant+Garamond|Proza+Libre|"
         <> "Istok+Web|Lora|"
         <> "Archivo+Black|Tenor+Sans|"
         <> "Roboto Mono|"
         <> "Cardo|Pathway+Gothic+One|"

blank_ :: Monad m => HtmlT m ()
blank_ = pure ()

javascript_ :: (ToHtml t, Monad m) => [Attribute] -> t -> HtmlT m ()
javascript_ attrs = script_ ((type_ "text/javascript") : attrs) 

async_javascript_ :: (ToHtml t, Monad m) => [Attribute] -> t -> HtmlT m ()
async_javascript_ attrs = javascript_ ((async_ "") : attrs) 

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
  async_javascript_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-MML-AM_CHTML"] blank_

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
          a_ [href_ "/"] "Mikrokosmos"
        nav_ navbarContents

      -- aside_ $ do 
      --   "aside"

      main_ [role_ "main"] $ do 
        "$body$"

      footer_ $ do
        a_ [href_ "/colophon"] "Colophon"
        " / "
        a_ [href_ "/colophon"] "Contact"
        " / "
        a_ [href_ "/colophon"] "Source"

navbarContents :: Html ()
navbarContents =
  ul_ $ do
    li_ (a_ [href_ "/about"] "Math")
    li_ (a_ [href_ "/about"] "Music")
    li_ (a_ [href_ "/about"] "Code")
    li_ (a_ [href_ "/about"] "About")

main :: IO ()
main = T.putStrLn (renderText template)
