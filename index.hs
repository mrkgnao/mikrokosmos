{-# LANGUAGE OverloadedStrings, CPP #-}
module Main (main) where

import Lucid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Data.Monoid ((<>))

main :: IO ()
main = T.putStrLn (renderText template)

template :: Html ()
template = do
  hakyllTitle "Soham Chowdhury"
  title_ "Mikrokosmos"
  intro

  h2_ [class_ "post-list__header"] "Essays"
  hakyllPartial "templates/post-list.html"

  -- h2_ [class_ "post-list__header"] "Around the web"
  -- hakyllPartial "templates/post-list.html"

intro :: Html ()
intro = div_ [class_ "intro"] $ do
  p_ [class_ "intro__para"] $ do
    "Hi! I'm Soham Chowdhury, a student of mathematics "
    "learning to coherently speak "
    "algebraic number theory and (eventually) arithmetic geometry."
  br_ []
  p_ [class_ "intro__para"] $ do
    "I'm also a functional programmer and advocate for "
    "expressive type systems; a self-taught guitarist and "
    "musician interested in counterpoint and jazz harmony; "
    "and an enthusiast of unexplained analogies and "
    "the endless unfamiliar." 
  

hakyllPartial :: Text -> Html ()
hakyllPartial p = toHtmlRaw ("$partial(\"" <> p <> "\")$")

hakyllField :: Text -> Text -> Html ()
hakyllField fld p = toHtmlRaw ("---\n" <> fld <> ": " <> p <> "\n---\n")

hakyllTitle :: Text -> Html ()
hakyllTitle = hakyllField "title"

