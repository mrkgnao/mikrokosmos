{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Clay
import Prelude hiding (div, rem)
import Control.Monad
import Data.Monoid ((<>))
import qualified Fonts

main :: IO ()
main = putCss myStylesheet

(~:) :: (a -> b) -> a -> b
(~:) = ($)

headlineFace, bodyFace :: Css
(headlineFace, bodyFace) = 
  (Fonts.lato, Fonts.merriweather)

mainColor :: Color
mainColor = "#d0f0f0"

headerColor :: Color
headerColor = "#f0d0f0"

footerColor :: Color
footerColor = "#f0f0d0"

paddingLR :: Size a -> Css
paddingLR p = do
  paddingLeft p
  paddingRight p

paddingUD :: Size a -> Css
paddingUD p = do
  paddingTop p
  paddingBottom p

myStylesheet :: Css
myStylesheet = do
  star ? do
    lineHeight (em 1.15)

  sbody
  sheader
  sfooter

  h1 <> h2 <> h3 <> h4 ? do
    -- headlineFace
    fontWeight bold
  -- h1 ? do
    -- fontSize (px 45)
    -- fontStyle italic
    -- textTransform uppercase

padCols :: Css
padCols = do
  paddingLeft (pct 11.111)
  paddingRight (pct 22.222)

sbody :: Css
sbody = body ? do
  color       black
  fontSize ~: px 16

  smain

smain :: Css
smain = main_ ? do
  -- alignSelf center
  -- width    ~: px 800
  -- sym2 margin (px 0) auto
  backgroundColor mainColor
  paddingUD (em 2)
  paddingLeft (pct 11.111)
  paddingRight (pct 25)

sheader :: Css
sheader = header ? do
  paddingUD ~: em 2
  -- borderBottom solid (px 1) black
  padCols
  -- backgroundColor headerColor

  -- boxSizing borderBox
  -- sym2 padding (px 12) 0
  h4 ? do
    float floatLeft
    marginTop (px 0)
  nav ? do
    -- sym padding (px 0)
    -- sym margin (px 0)
    marginLeft (pct 25)
    ul ? do
      marginTop (em 0)
      li ? do 
        display inline
        listStyleType none
        a ? do
          paddingLR (em 2)
          -- pure ()

sfooter :: Css
sfooter = footer ? do
  padCols
  paddingUD ~: em 2
  -- backgroundColor footerColor

  -- borderTop solid (px 1) black
