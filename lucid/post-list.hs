{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lucid
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Data.Monoid ((<>))

main :: IO ()
main = T.putStrLn (renderText template)

template :: Html ()
template = do
  plist "posts" "Articles"
  plist "drafts" "Drafts"

  where
    plist :: Html () -> Html () -> Html ()
    plist var title = do
      "$if(" <> var <>")$"
      h2_ [class_ "post-list__header"] title
      hfor var $ do
        div_ [class_ "post-link__title"] $
          a_ [href_ "$url$"] "$title$"
        div_ [class_ "post-link__date"] "$date$"
      "$else$"
      "$endif$"

    hfor :: Html() -> Html () -> Html ()
    hfor var body = do
      "$for(" <> var <> ")$"
      body
      "$endfor$"

