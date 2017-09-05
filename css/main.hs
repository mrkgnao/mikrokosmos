{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Clay
import Prelude hiding (div)

main :: IO ()
main = putCss myStylesheet

(~:) :: (a -> b) -> a -> b
(~:) = ($)

myStylesheet :: Css
myStylesheet = do
  sbody
  sheader
  sfooter

sbody :: Css
sbody = body ? do
  color       black
  fontSize ~: px 16
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
