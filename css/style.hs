{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Clay
import Prelude hiding (div)
import Control.Monad
import Data.Monoid ((<>))

main :: IO ()
main = putCss myStylesheet

(~:) :: (a -> b) -> a -> b
(~:) = ($)

sansSerifFont f = fontFamily [f] [sansSerif]
serifFont f = fontFamily [f] [serif]
cursiveFont f = fontFamily [f] [cursive]

unicaOne          = cursiveFont "Unica One"

archivoBlack      = sansSerifFont "Archivo Black"
cormorantGaramond = sansSerifFont "Cormorant Garamond"
istokWeb          = sansSerifFont "Istok Web"
juliusSansOne     = sansSerifFont "Julius Sans One"
lato              = sansSerifFont "Lato"
libreFranklin     = sansSerifFont "Libre Franklin"
monda             = sansSerifFont "Monda"
openSans          = sansSerifFont "Open Sans"
pathwayGothicOne  = sansSerifFont "Pathway Gothic One"
questrial         = sansSerifFont "Questrial"
robotoSans        = sansSerifFont "Roboto Sans"
rubik             = sansSerifFont "Rubik"
tenor             = sansSerifFont "Tenor Sans"
workSans          = sansSerifFont "Work Sans"

alegreya          = serifFont "Alegreya"
arvo              = serifFont "Arvo"
cardo             = serifFont "Cardo"
crimsonText       = serifFont "Crimson Text"
libreBaskerville  = serifFont "Libre Baskerville"
lora              = serifFont "Lora"
merriweather      = serifFont "Merriweather"
neuton            = serifFont "Neuton"
prozaLibre        = serifFont "Proza Libre"
trirong           = serifFont "Trirong"

headlineFace, bodyFace :: Css
(headlineFace, bodyFace) = 
  (lato, merriweather)
  -- (rubik, trirong)
  -- (libreFranklin, libreBaskerville)
  -- (cormorantGaramond, prozaLibre)
  -- (istokWeb, lora)
  -- (lato, robotoSans)
  -- (workSans, openSans)
  -- (unicaOne, crimsonText)
  -- (juliusSansOne, monda)
  -- (neuton, workSans)
  -- (archivoBlack, tenor)
  -- (pathwayGothicOne, cardo)
  -- (workSans, openSans)
  -- (questrial, alegreya)

myStylesheet :: Css
myStylesheet = do
  sbody
  sheader
  sfooter
  star ? bodyFace
  h1 <> h2 <> h3 <> h4 ? do
    headlineFace
    fontWeight bold
  h1 ? do
    fontSize (px 45)
    -- fontStyle italic
    -- textTransform uppercase

sbody :: Css
sbody = body ? do
  color       black
  fontSize ~: px 16

  main_ ? do
    alignSelf center
    width    ~: px 800
    sym2 margin (px 0) auto

sfooter :: Css
sfooter = footer ? do
  marginTop ~: px 30
  paddingTop ~: px 20
  borderTop solid (px 1) black

sheader :: Css
sheader = header ? do
  width (pct 100)
  marginBottom ~: px 30

  -- boxSizing borderBox
  borderBottom solid (px 1) black
  -- sym2 padding (px 12) 0
  h4 ? do
    float floatLeft
    sym margin (px 0)
    width (px 200)
  nav ? do
    -- sym padding (px 0)
    -- sym margin (px 0)
    ul ? 
      li ? do 
        display inline
        listStyleType none
        a ? do
          padding (px 23) 8 23 8
