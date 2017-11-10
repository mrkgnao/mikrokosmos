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
  (Fonts.rubik, Fonts.roboto)

monospaceFace :: Css
monospaceFace = Fonts.robotoMono

bem :: Text -> Text -> Text -> Selector
bem block elem mod = text ("." <> block <> "__" <> elem <> "--" <> mod)

be :: Text -> Text -> Selector
be block elem = text ("." <> block <> "__" <> elem)

bm :: Text -> Text -> Selector
bm block mod = text ("." <> block <> "--" <> mod)

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

highlightCss :: Css
highlightCss = ".sourceCode" ? do
  -- keyword
  "span.kw" ? do
     color "#007020" 
     fontWeight bold
  -- datatype
  "span.dt" ? 
    color "#902000"
  "span.co" ? do
    color "#60a0b0" 
    -- fontStyle italic
  "span.ot" ? color "#007020"
  "span.al" ? do
    color red 
    fontWeight bold
  "span.fu" ? color "#06287e"
  "span.re" ? pure ()
  "span.dv" ? color "#40a070"
  "span.bn" ? color "#40a070"
  "span.fl" ? color "#40a070"
  "span.ch" ? color "#4070a0"
  "span.st" ? color "#4070a0"
  "span.er" ? do
    color red
    fontWeight bold

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

  pre <> code ? do
    monospaceFace
    smallScreen <> mediumScreen $
      overflowX scroll

  highlightCss

  h1 <> h2 <> h3 <> h4 ? do
    headlineFace
    fontWeight bold
    lineHeight (em 1.2)
    marginTop (em 1.5)

  ".index__header" ? do
    marginTop (rem 2)
    paddingBottom (em 0.5)

  h1 ? fontSize (px 28)
  h2 ? fontSize (px 24)
  h3 ? fontSize (px 20)
  h4 ? fontSize (px 18)

  ol ?
    listStyleType decimal

  sup ? do
    verticalAlign vAlignSuper
    fontSize (pct 80)

  ul ? do
    paddingLeft (px 20)

  a ? do
    textDecoration none
    outlineStyle none
    sym2 padding (px 2) (px 2)
    sym2 margin (px 0) (px (-2))

    let colors = color linkCol
    mapM_ (& colors) [link, visited, focus]
    hover & do
      color "#311f6d"
      backgroundColor "#e2dbff"
  
  be "post-header" "title" ? do
    fontSize (px 32)
    marginTop (em 0)
    marginBottom (em 1)
    paddingBottom (px 4)
    borderBottom solid (px 2) black

  be "post-header" "date" ? do
    fontSize (px 14)
    marginBottom (em 0.5)


  pre ? do
    marginTop (em 2)
    marginBottom (em 2)
    paddingLeft (em 1)
    paddingRight (em 1)

  (ul <> ol) ? do
    paddingLeft (em 2)

  p ? do
    marginTop (em 0.5)
    marginBottom (em 0.5)

  blockquote ? do
    -- fontStyle italic
    bigScreen $ paddingLeft (pct 20)
    smallScreen $ do
      fontSize (px 16)
      lineHeight (em 1.3)
    marginTop (em 1)
    marginBottom (em 1)

  "div.figure" ? do
    smallScreen $ do
       marginLeft (pct (-12.2))
       marginRight (pct (-11.0))

    mediumScreen $ do
       marginLeft (pct (-13))
       marginRight (pct (-11))

    bigScreen $ do
       marginLeft (pct (-33))
       marginRight (pct (-29))

  -- "img.inline-math" ? do
    -- height (em 1)
    -- width auto
    --
  "img.display-math" ? do
    textAlign center

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

data ScreenSize = Small | Medium | Big
data ScreenSide = L | R

-- bigScreenMarginLeft = pct 32
-- bigScreenMarginRight = pct 15

marginFor :: ScreenSize -> ScreenSide -> Size Percentage
marginFor Big L = pct 35
marginFor Big R = pct 10

marginFor Medium L = pct 20
marginFor Medium R = pct 10

marginFor Small L = pct 4
marginFor Small R = marginFor Small L

padByMargins sz = paddingLeft (marginFor sz L) *> paddingRight (marginFor sz R)
padByMarginsQ sz = screenSizeQuery sz (padByMargins sz)

screenSizeQuery Big = query Clay.all [M.minWidth 1201]
screenSizeQuery Medium = query Clay.all [M.minWidth 801, M.maxWidth 1200]
screenSizeQuery Small = query Clay.all [M.maxWidth 800]

bigScreen = screenSizeQuery Big
mediumScreen = screenSizeQuery Medium
smallScreen = screenSizeQuery Small

smain :: Css
smain = 
  main_ ? do
  -- alignSelf center
  -- width    ~: px 800
  -- sym2 margin (px 0) auto
  ol ? li ? do
    paddingUD (em 0.3)

  hr ? do
    sym2 margin (em 3) auto
    width (em 2)

  fontSize ~: px 16
  lineHeight ~: em 1.5

  -- Important to use rem here
  marginTop (em 2)
  paddingBottom (em 2)
  maxWidth (px 900)

  "em" ? do
    paddingRight (px 2)

  mapM padByMarginsQ [Big, Medium, Small]

  -- maxWidth (px 600)

  h2 ? do
    marginTop ~: em 2
    marginBottom ~: em 0.5

  be "post-link" "date" ? do
    fontSize (px 14)

  be "post-link" "title" ? do
    marginTop (em 1)

sheader :: Css
sheader = header ? do
  -- paddingTop (em 0.5)
  bigScreen $ do
    float floatLeft
    maxWidth (marginFor Big L)
  -- borderBottom solid (px 1) black
  -- backgroundColor headerColor

  -- boxSizing borderBox
  -- sym2 padding (px 12) 0
  -- a ? do
    -- link & borderBottom solid (px 2) black
  -- h3 ? do
    -- bigScreen $ float floatLeft
    -- fontSize (px 24)
    -- marginLeft (pct 10)
    -- marginTop (em 2)
    -- a ? do
    --   border solid (px 2) white

    --   sym2 padding (px 4) (px 25)
    --   sym2 margin (px 0) (px (-20))

    --   backgroundColor linkCol
    --   let colors = color white
    --   mapM_ (& colors) [link, visited, focus]
    --   hover & do
    --     color linkCol
    --     backgroundColor white
    --     border solid (px 2) linkCol

  p ? do
    fontSize (px 16)
    width (pct 80)
    maxWidth (px 500)
    lineHeight (em 1.4)
    sym2 margin (em 1) (px 7)

  h3 <> h4 ? do
    fontSize (px 16)
    sym2 margin (rem 1) (em 0)
    sym2 padding (rem 0) (em 0)

    a ? do
      let fg = white
      let bg = linkCol
      color fg
      backgroundColor bg

      -- alternatively:
      -- borderLeft solid (px 2) white
      -- borderBottom solid (px 2) linkCol

      border solid (px 2) fg

      let colors = color fg
      mapM_ (& colors) [link, visited, focus]

      hover & do
        color bg
        backgroundColor fg
        border solid (px 2) bg

  h3 ? a ? do 
    sym2 padding (px 4) (px 30)
    sym2 margin (px 0) (px (-25))

  mediumScreen <> bigScreen $ do
    h4 ? a ? do
      sym2 padding (px 4) (px 30)
      sym2 margin (px 0) (px (-25))

  smallScreen $ do
    nav ? do
      sym2 margin (em 1) auto
      textAlign (alignSide sideRight)
      h4 ? do
        display inline
        -- sym2 padding (em 1) (em 1)
        sym2 margin (em 1) (px 3)
        a ? sym padding (px 5)
      -- sym2 margin (px 0) (px (-25))

  h3 ? do
    -- bigScreen $ float floatLeft
    fontSize (px 24)

  -- nav ? do
  --   -- sym padding (px 0)
  --   -- sym margin (px 0)
  --   do 
  --     smallScreen (display none)
  --     mediumScreen (display none)
  --     bigScreen $ do
  --       -- marginLeft bigScreenMarginLeft
  --       headlineFace
  --       ul ? do
  --         sym padding (px 0)
  --         width (pct 60)
  --         maxWidth (px 600)
  --         display flex
  --         justifyContent spaceBetween
  --         marginTop (em 0)
  --         li ? do 
  --           flexAll 0 0 auto
  --           -- display inline
  --           listStyleType none

sfooter :: Css
sfooter = do
  footer ? do
     bodyFace
     fontSize (px 16)
     paddingUD ~: em 2
  "footer.page-footer" ? do
     fontSize (px 12)
     bigScreen $ marginLeft (marginFor Big L)
     mediumScreen $ marginLeft (marginFor Medium L)
     smallScreen $ marginLeft (marginFor Small L)
  -- backgroundColor footerColor

  -- borderTop solid (px 1) black
