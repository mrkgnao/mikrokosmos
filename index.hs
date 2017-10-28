{-# LANGUAGE OverloadedStrings, CPP, FlexibleContexts #-}
module Main (main) where

import Lucid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text as TS
import Data.Monoid ((<>))

main :: IO ()
main = T.putStrLn (renderText template)

template :: Html ()
template = do
  hakyllTitle "Soham Chowdhury"
  title_ "Mikrokosmos"

  h1_ [class_ "index__header"] "Hello there!"
  intro

  h2_ [class_ "post-list__header"] "Projects"
  projects
  
  hakyllPartial "templates/post-list.html"

  h2_ [class_ "post-list__header"] "Notes and short proofs of concept"
  otherCode

  h2_ [class_ "post-list__header"] "Greatest hits from around the web"
  links

intro :: Html ()
intro = div_ [class_ "intro"] $ do
  p_ [class_ "intro__para"] $ do
    "I'm Soham Chowdhury, a student of mathematics "
    "learning to coherently speak "
    "algebraic number theory and (eventually) arithmetic geometry."
  p_ [class_ "intro__para"] $ do
    "I'm also a functional programmer interested in programming-language "
    "theory and expressive type systems, a self-taught guitarist and "
    "musician interested in counterpoint and functional harmony; a "
    "fan of a lot of "
    a_ [href_ "/things-i-like#music"] "music"
    " and "
    a_ [href_ "/things-i-like#writing"] "writing"
    "; "
    "and an enthusiast for unexplained analogies in and around mathematics."
    
  p_ [class_ "intro__para"] $ do
    "I'm interested "
    "in and like discussing a variety of things, "
    "including literature, music (theory), "
    "physics, philosophy, "
    "and cognitive science." 

  p_ [class_ "intro__para"] $ do
    "This is my personal site, named after "
    "the "
    a_ [href_ "https://en.wikipedia.org/wiki/Mikrokosmos_(Bart%C3%B3k)"] 
       "Béla Bartók work"
    "."

projects :: Html ()
projects = div_ $ do
  mapM_ mkLink $ 
    [ ( "sound-and-complete"
      , "https://github.com/mrkgnao/sound-and-complete"
      , do
          "A work-in-progress implementation of the \"Sound and Complete\" "
          "type system from " 
          a_ [href_ "https://arxiv.org/abs/1601.05106"] "Dunfield and Krishnaswami (2016)"
          ", "
          "which describes a minimal ML-like language with GADTs."
      )
    , ( "noether"
      , "https://github.com/mrkgnao/noether"
      , do 
          "A numeric programming framework for Haskell featuring "
          "highly polymorphic algebraic structures and custom deriving strategies to "
          "build complex algebraic behaviors from simpler ones."
      )
    ]
  
  where 
    mkLink :: (Html (), TS.Text, Html ()) -> Html ()
    mkLink (title, link, desc) = div_ [] $ do
      h3_ (a_ [href_ link] title)
      p_ [] desc

otherCode :: Html ()
otherCode = div_ [class_ "links"] $ do
  mapM_ mkLink code 

  where
  code = 
    [ ( "https://gist.github.com/mrkgnao/1a5c06cc1e188f2238c8c11cb74efe9a"
      , "A stripped-down Todo app using the Reflex FRP library, with lots of comments"
      )
    , ( "https://gist.github.com/mrkgnao/ca8ba5b22a8cb95e24179b18b8516ed6"
      , "A peculiar free arrow type"
      )
    , ( "https://gist.github.com/mrkgnao/a45059869590d59f05100f4120595623"
      , "Type-safe neural networks in Idris, with compiler-checked linear algebra!"
      )
    , ( "https://news.ycombinator.com/item?id=14776588"
      , "The quest to learn Teichmüller theory in honor of Mirzakhani's memory" )
    ]

links :: Html ()
links = div_ [class_ "links"] $ do
  mapM_ mkLink other 
  mapM_ mkLinkHn hn 

  where
  other = 
    [ ( "https://www.reddit.com/r/haskell/comments/765ogm/multiple_type_class_instances_for_the_same_type/doce727/"
      , "How I allow multiple monoid, ring, etc. structures in Noether using type families")
    , ( "https://www.reddit.com/r/haskell/comments/786f1i/the_disciplined_disciple_compiler_v051/dotbz0e/"
      , "What are higher-rank polymorphism and bidirectional type inference?")
    ]

  hn = 
    [ ( "15424273"
      , "The Langlands program in number theory")
    , ( "15008848"
      , "Diatonic chord construction 101")
    , ( "14934846"
      , "The average Haskell user knows much less category theory than you think")
    , ( "15365557"
      , "Learn extreme pointfree notation, but don't use it")
    , ( "14459459"
      , "Data families and code reuse ")
    , ( "14459070"
      , "Rewrite rules for high-performance Haskell (with examples from real libraries) ")
    , ( "14149200"
      , "The Servant library, the compiler as a companion, and evaluating your Haskell proficiency ")
    , ( "14016396"
      , "What \"prime\" means, and Fermat's theorem on sums of two squares")
    , ( "14017178"
      , "Ideals are better than elements!")
    , ( "13940790"
      , "No, you're not stupid: math is actually hard")
    , ( "13907573"
      , "Thunks and laziness in Haskell")
    , ( "13905459"
      , "Idle thoughts about abstraction in math and pedagogy")
    , ( "13874025"
      , "Haskell's terse syntax isn't all that bad")
    , ( "13678111"
      , "ELI5: what is a sheaf? ")
    ]

  mkLinkHn (link, desc) = p_ (a_ [href_ ("https://news.ycombinator.com/item?id=" <> link)] desc)

mkLink (link, desc) = p_ (a_ [href_ link] desc)

hakyllPartial :: Text -> Html ()
hakyllPartial p = toHtmlRaw ("$partial(\"" <> p <> "\")$")

hakyllField :: Text -> Text -> Html ()
hakyllField fld p = toHtmlRaw ("---\n" <> fld <> ": " <> p <> "\n---\n")

hakyllTitle :: Text -> Html ()
hakyllTitle = hakyllField "title"
