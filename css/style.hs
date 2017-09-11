{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Clay hiding (map)
import Prelude hiding (div, rem)
import Data.Text (Text)
import Control.Monad
import Data.Monoid ((<>))
import qualified Fonts
import Clay.Selector (text)

import qualified Clay.Media as M

main :: IO ()
main = putCss myStylesheet

(~:) :: (a -> b) -> a -> b
(~:) = ($)

headlineFace, bodyFace :: Css
(headlineFace, bodyFace) = 
  (Fonts.rubik, Fonts.rubik)

bem :: Text -> Text -> Text -> Selector
bem block elem mod = text ("." <> block <> "__" <> elem <> "--" <> mod)

be :: Text -> Text -> Selector
be block elem = text ("." <> block <> "__" <> elem)

bm :: Text -> Text -> Selector
bm block mod = text ("." <> block <> "--" <> mod)

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

linkCol :: Color
linkCol = "#481cd8"

myStylesheet :: Css
myStylesheet = do
  html ? do
    lineHeight (em 1.15)
    bodyFace
    fontSize ~: px 16
    color       black

  sbody
  sheader
  sfooter

  h1 <> h2 <> h3 <> h4 ? do
    headlineFace
    fontWeight bold
    lineHeight (em 1.2)

  ul ? do
    paddingLeft (px 20)

  a ? do
    textDecoration none
    outlineStyle none
    sym2 padding (px 2) (px 0)

    let colors = color linkCol
    mapM_ (& colors) [link, visited, focus]
    hover & do
      color "#311f6d"
      backgroundColor "#82abff"
  
  be "post-header" "title" ? do
    fontSize (px 28)
    marginBottom (em 2)
    paddingBottom (px 4)
    borderBottom solid (px 2) black

  be "post-header" "date" ? do
    fontSize (px 16)
    marginBottom (em 0.5)

  code ? do
    fontFamily [] [monospace]

  pre ? do
    marginTop (em 2)
    marginBottom (em 2)
    paddingLeft (em 2)

  (ul <> ol) ? do
    paddingLeft (em 2)

  p ? do
    marginTop (em 0.5)
    marginBottom (em 0.5)

  blockquote ? do
    fontStyle italic
    bigScreen $ paddingLeft (pct 20)
    smallScreen $ do
      fontSize (px 16)
      lineHeight (em 1.3)

  "div.figure" ? do
    smallScreen $ do
       marginLeft (pct (-13))
       marginRight (pct (-11))

    mediumScreen $ do
       marginLeft (pct (-33))
       marginRight (pct (-29))

    bigScreen $ do
       marginLeft (pct (-42))
       marginRight (pct (-35))

  img ? do
    width (pct 100)

-- padCols :: Css
-- padCols = do
--   paddingLeft (pct 11.111)
--   paddingRight (pct 22.222)

sbody :: Css
sbody = do
  smain

flexAll a b c = do
  flexGrow a
  flexShrink b
  flexBasis c

smain :: Css
smain = do
  main_ ? do
    -- alignSelf center
    -- width    ~: px 800
    -- sym2 margin (px 0) auto
    fontSize ~: px 20
    lineHeight ~: em 1.25
    -- backgroundColor mainColor
    paddingUD (em 2)

    bigScreen $ do 
      paddingLeft (pct 30)
      paddingRight (pct 27)

    mediumScreen $ do 
      paddingLeft (pct 20)
      paddingRight (pct 18)

    smallScreen $ do 
      paddingLeft (pct 10)
      paddingRight (pct 9)
    -- maxWidth (px 600)

  h2 ? do
    marginTop ~: em 2
    marginBottom ~: em 0.5

  be "post-link" "date" ? do
    fontSize (px 14)

  be "post-link" "title" ? do
    marginTop (em 1)

smallScreen = query Clay.all [M.maxWidth 800]
mediumScreen = query Clay.all [M.minWidth 801, M.maxWidth 1200]
bigScreen = query Clay.all [M.minWidth 1201]

sheader :: Css
sheader = header ? do
  paddingUD ~: em 2
  -- borderBottom solid (px 1) black
  -- backgroundColor headerColor

  -- boxSizing borderBox
  -- sym2 padding (px 12) 0
  a ? do
    link & borderBottom solid (px 0) black
  fontSize (px 20)
  h4 ? do
    bodyFace
    float floatLeft
    marginTop (px 0)
    marginLeft (pct 10)

  nav ? do
    -- sym padding (px 0)
    -- sym margin (px 0)
    do 
      smallScreen (display none)
      mediumScreen (display none)
      bigScreen $ do
        marginLeft (pct 30)
        headlineFace
        ul ? do
          sym padding (px 0)
          width (pct 60)
          maxWidth (px 600)
          display flex
          justifyContent spaceBetween
          marginTop (em 0)
          li ? do 
            flexAll 0 0 auto
            display inline
            listStyleType none

sfooter :: Css
sfooter = footer ? do
  bodyFace
  fontSize (px 16)
  paddingUD ~: em 2
  bigScreen $ marginLeft (pct 30)
  mediumScreen $ marginLeft (pct 20)
  smallScreen $ marginLeft (pct 10)
  -- backgroundColor footerColor

  -- borderTop solid (px 1) black
