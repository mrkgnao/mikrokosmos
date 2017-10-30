--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
import           Data.Monoid ((<>))
import           Hakyll
import           Data.List              (sortBy,isSuffixOf)
import           System.FilePath.Posix  (takeBaseName,takeDirectory,(</>))
import Data.Typeable
import Data.Binary

--------------------------------------------------------------------------------
imagesDir = "images"
cssDir = "css"
postsDir = "posts"
draftsDir = "drafts"
templatesDir = "templates"

matchGlob root glob = match (fromGlob (root </> glob))

staticPat :: Pattern
staticPat = fromList ["about.md", "things-i-like.md", "contact.markdown"]

routeWith
  :: (Writable a, Typeable a, Binary a)
  => Routes
  -> Compiler (Item a)
  -> Rules ()
routeWith rt cpr = route rt *> compile cpr

idRouteWith
  :: (Writable a, Typeable a, Binary a) => Compiler (Item a) -> Rules ()
idRouteWith = routeWith idRoute

cleanRouteWith
  :: (Writable a, Typeable a, Binary a) => Compiler (Item a) -> Rules ()
cleanRouteWith = routeWith cleanRoute

idRouteCopy :: Rules ()
idRouteCopy = idRouteWith copyFileCompiler

main :: IO ()
main = hakyll site

site :: Rules ()
site = do
  matchGlob imagesDir "*"     idRouteCopy
  matchGlob cssDir    "*.css" (idRouteWith compressCssCompiler)
  match "css/*.hs" $ routeWith
    (setExtension "css")
    (getResourceString >>= withItemBody (unixFilter "runghc" []))
  match  staticPat        staticPages
  match  "posts/**"       postRules
  match  "drafts/**"      postRules
  create ["archive.html"] archivePage
  match  "index.html"     indexPage
  matchGlob templatesDir "*.html" (compile templateBodyCompiler)

staticPages =
  cleanRouteWith
    $   pandocCompiler
    >>= loadAndApplyTemplate "templates/Default.html" defaultContext
    >>= relativizeUrls

archivePage = idRouteWith $ do
  let skipIfEmpty d t = if null d then mempty else listField t postCtx (pure d)
  posts  <- loadAll "posts/*" >>= recentFirst
  drafts <- loadAll "drafts/*" >>= recentFirst
  let archiveCtx =
        skipIfEmpty posts "posts"
          <> skipIfEmpty drafts  "drafts"
          <> constField  "title" "Archives"
          <> defaultContext

  makeItem ""
    >>= loadAndApplyTemplate "templates/Archive.html" archiveCtx
    >>= loadAndApplyTemplate "templates/Default.html" archiveCtx
    >>= relativizeUrls


indexPage = idRouteWith $ do
  let skipIfEmpty d t = if null d then mempty else listField t postCtx (pure d)
  posts  <- loadAll "posts/*" >>= recentFirst
  drafts <- loadAll "drafts/*" >>= recentFirst
  let indexCtx =
        skipIfEmpty posts "posts"
          <> skipIfEmpty drafts  "drafts"
          <> constField  "title" "Home"
          <> defaultContext

  getResourceBody
    >>= applyAsTemplate indexCtx
    >>= loadAndApplyTemplate "templates/Default.html" postCtx
    >>= relativizeUrls
    >>= cleanIndexUrls
    >>= cleanIndexHtmls

postRules :: Rules ()
postRules =
  cleanRouteWith
    $   pandocCompiler
    >>= loadAndApplyTemplate "templates/Post.html"    postCtx
    >>= loadAndApplyTemplate "templates/Default.html" postCtx
    >>= relativizeUrls

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
 where
  createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
    where p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
 where
  pattern     = "/index.html"
  replacement = const "/"

cleanIndex :: String -> String
cleanIndex url | idx `isSuffixOf` url = take (length url - length idx) url
               | otherwise            = url
  where idx = "index.html"

-- match "posts/*" $ version "raw" $ do
--     route idRoute
--     compile copyFileCompiler

