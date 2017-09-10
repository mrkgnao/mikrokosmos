{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lucid
import qualified Data.Text.Lazy.IO as T

main :: IO ()
main = T.putStrLn (renderText template)

template :: Html ()
template =
  article_ $ do
    div_ [class_ "post-header"] $ do
      div_ [class_ "post-header__date"] "$date$"
      h1_ [class_ "post-header__title"] "$title$"
    section_ "$body$"
