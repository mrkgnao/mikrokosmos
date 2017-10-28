{-# LANGUAGE TypeOperators, NoMonomorphismRestriction #-}
module Templates.Common where

-- import Lucid
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Data.Monoid ((<>))

import Html
import Html.Attribute as A

infixr 5 !
(!) = (#)
