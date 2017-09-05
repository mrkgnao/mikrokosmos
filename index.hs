{-# LANGUAGE OverloadedStrings #-}
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
  title_ "Imagined Saviors"
  p_ intro

  h2_ "Essays"
  hakyllPartial "templates/post-list.html"

intro :: Html ()
intro = (toHtml . T.concat) 
  [ "I'm a student of mathematics learning to coherently speak "
  , "algebraic number theory and (eventually) arithmetic geometry, "
  , "a functional programmer with an interest in "
  , "expressive type systems and homotopy type theory, " 
  , "and an enthusiast of unexplained analogies and the endless unfamiliar." 
  ]

hakyllPartial :: Text -> Html ()
hakyllPartial p = toHtmlRaw ("$partial(\"" <> p <> "\")$")

hakyllField :: Text -> Text -> Html ()
hakyllField fld p = toHtmlRaw ("---\n" <> fld <> ": " <> p <> "\n---\n")

hakyllTitle :: Text -> Html ()
hakyllTitle = hakyllField "title"

