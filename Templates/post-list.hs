{-# LANGUAGE TypeOperators, NoMonomorphismRestriction #-}
module Main (main) where

-- import Lucid
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Data.Monoid ((<>))

import Html
import Html.Attribute as A
import Templates.Common

main :: IO ()
main = T.putStrLn (renderText template)

template 
  = plist "posts" "Articles"
  ! plist "drafts" "Drafts"

h2' = h2_A
div' = div_A
a' = a_A

withClass f k b = f (class_ k) b

h2C = withClass h2'
divC = withClass div'

plist var title 
  = hif var
    ( h2C "post-list__header" title 
    ! hfor var
        ( divC "post-link__title"
            (a' (href_ (hdol "url")) 
                (hdol "title"))
        ! divC "post-link__date"
            (hdol "date")
        )
    ) 

-- | Wrap a keyword in dollar signs.
hdol x = "$" <> x <> "$"

-- | Abstracts the common pattern of a keyword followed by a parenthesised
-- parameter.
hkw kw v = hdol (kw <> hparens v)

-- | Wrap something in parentheses.
hparens x = "(" <> x <> ")"

-- | Hakyll `for` expression.
hfor var body 
  = hkw "for" var
  ! body 
  ! hdol "endfor"

-- | Hakyll `if-then-else` expression.
hifelse var trueBody falseBody
  = hdol ("if" <> hparens var)
  ! trueBody
  ! hdol "else"
  ! falseBody
  ! hdol "endif"

hif var trueBody
  = hifelse var trueBody ()
