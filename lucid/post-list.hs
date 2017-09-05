{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lucid
import qualified Data.Text.Lazy.IO as T

main :: IO ()
main = T.putStrLn (renderText template)

template :: Html ()
template = ul_ $ do
  "$for(posts)$"
  li_ $ do
    a_ [href_ "$url$"] "$title$"
    " - "
    "$date$"
  "$endfor$"
