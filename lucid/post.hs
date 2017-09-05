{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lucid
import qualified Data.Text.Lazy.IO as T

main :: IO ()
main = T.putStrLn (renderText template)

template :: Html ()
template =
  article_ $ do
    section_ [class_ "header"] $ do
      "Posted on $date$"
      "$if(author)$"
      "by $author$"
      "$endif$"
    section_ "$body$"
