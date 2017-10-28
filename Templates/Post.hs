module Templates.Post where

import Templates.Internal.Common

main :: IO ()
main = defaultMain template

-- | Post/draft template.
template = article_
  ( divC
      "post-header"
      ( divC "post-header__date"  (hdol "date")
      ! h1C  "post-header__title" (hdol "title")
      )
  ! section_ (hdol "body")
  )
