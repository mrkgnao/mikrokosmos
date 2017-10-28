{-# LANGUAGE TypeOperators, NoMonomorphismRestriction #-}
module Templates.PostList where

import Data.Monoid ((<>))

import Html
import Html.Attribute as A
import Templates.Internal.Common

main :: IO ()
main = defaultMain template

template 
  = plist "posts" "Articles"
  ! plist "drafts" "Drafts"

plist var title = 
  hif var
    ( h2C "post-list__header" title 
    ! hfor var
        ( divC "post-link__title"
            (a' (href_ (hdol "url")) 
                (hdol "title"))
        ! divC "post-link__date"
            (hdol "date")
        )
    ) 
