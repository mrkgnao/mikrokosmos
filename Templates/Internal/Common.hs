{-# LANGUAGE TypeOperators, NoMonomorphismRestriction, FlexibleContexts #-}
module Templates.Internal.Common 
  ( module Html
  , module Html.Attribute
  , defaultMain
  , (!)
  -- Hakyll syntax
  , hdol
  , hkw
  , hparens
  -- Control structures
  , hifelse
  , hif
  , hfor
  -- type-of-html
  , a'
  , div'
  , h1'
  , h2'
  , h3'
  , h4'
  , h1C
  , h2C
  , h3C
  , h4C
  , divC
  ) where

-- import Lucid
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Data.Monoid ((<>))

import Html
import Html.Attribute hiding 
  ( summary_ , title_ , style_ , slot_
  , label_   , form_  , dir_   , data_
  , content_ , span_  , cite_  , code_
  )

infixr 5 !
(!) = (#)

defaultMain template = T.putStrLn (renderText template)

--------------------------------------------------------------------------------
-- Hakyll utilities
--------------------------------------------------------------------------------

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

-- | Hakyll `if-then` expression.
hif var trueBody
  = hifelse var trueBody ()

--------------------------------------------------------------------------------
-- type-of-html utilities
--------------------------------------------------------------------------------

a' = a_A
div' = div_A

h1' = h1_A
h2' = h2_A
h3' = h3_A
h4' = h4_A

withClass f k b = f (class_ k) b

h1C = withClass h1'
h2C = withClass h2'
h3C = withClass h3'
h4C = withClass h4'

divC = withClass div'
