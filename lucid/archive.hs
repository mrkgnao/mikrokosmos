{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lucid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as T

main :: IO ()
main = T.putStrLn (renderText template)

template :: Html ()
template = do
  "Here you can find all my previous posts!"
  toHtmlRaw ("$partial(\"templates/post-list.html\")$" :: Text)

