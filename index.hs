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
  p_ intro

  h2_ "Essays"
  hakyllPartial "templates/post-list.html"

intro :: Html ()
intro = do
  "Hi! I'm Soham Chowdhury, a student of mathematics \
  learning to coherently speak \
  algebraic number theory and (eventually) arithmetic geometry. "
  br_ []
  br_ []
  "I'm also a functional programmer and advocate of \
  expressive type systems, an amateur \
  musician with an interest in harmony and composition, \
  and an enthusiast of unexplained analogies and the endless unfamiliar." 
  

hakyllPartial :: Text -> Html ()
hakyllPartial p = toHtmlRaw ("$partial(\"" <> p <> "\")$")

hakyllField :: Text -> Text -> Html ()
hakyllField fld p = toHtmlRaw ("---\n" <> fld <> ": " <> p <> "\n---\n")

hakyllTitle :: Text -> Html ()
hakyllTitle = hakyllField "title"

