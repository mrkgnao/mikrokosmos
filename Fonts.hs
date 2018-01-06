{-# LANGUAGE OverloadedStrings #-}
module Fonts where

import Clay

sansSerifFont f = fontFamily [f] [sansSerif]
serifFont f = fontFamily [f] [serif]
cursiveFont f = fontFamily [f] [cursive]
monospaceFont f = fontFamily [f] [monospace]

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
roboto            = sansSerifFont "Roboto"
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

robotoMono        = monospaceFont "Roboto Mono"
oxygenMono        = monospaceFont "Oxygen Mono"

iosevka = monospaceFont "Iosevka"
