{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lucid
import qualified Data.Text.Lazy.IO as T

main :: IO ()
main = T.putStrLn (renderText template)

template :: Html ()
template = do
  "$for(posts)$"
  div_ [class_ "post-link__title"] $ a_ [href_ "$url$"] "$title$"
  div_ [class_ "post-link__date"] "$date$"
  "$endfor$"
