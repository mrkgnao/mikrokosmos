--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
import           Data.Monoid ((<>))
import           Hakyll
import           Data.List              (sortBy,isSuffixOf)
import           System.FilePath.Posix  (takeBaseName,takeDirectory,(</>))

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.hs" $ do
        route   $ setExtension "css"
        compile $ getResourceString >>= withItemBody (unixFilter "runghc" [])

    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.md", "things-i-like.md", "contact.markdown"]) $ do
        route   $ cleanRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/**" $ do
        route cleanRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "drafts/**" $ do
        route cleanRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    -- match "posts/*" $ version "raw" $ do
    --     route idRoute
    --     compile copyFileCompiler

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            let skipIfEmpty d t = if null d then mempty else listField t postCtx (pure d)
            posts <- loadAll "posts/*" >>= recentFirst
            drafts <- loadAll "drafts/*" >>= recentFirst
            let archiveCtx =
                    skipIfEmpty posts "posts" <>
                    skipIfEmpty drafts "drafts" <>
                    constField "title" "Archives" <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            let skipIfEmpty d t = if null d then mempty else listField t postCtx (pure d)
            posts <- loadAll "posts/*" >>= recentFirst
            drafts <- loadAll "drafts/*" >>= recentFirst
            let indexCtx =
                    skipIfEmpty posts "posts" <>
                    skipIfEmpty drafts "drafts" <>
                    constField "title" "Home"                <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls
                >>= cleanIndexUrls
                >>= cleanIndexHtmls

    match "templates/*.html" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p 
                          </> takeBaseName p 
                          </> "index.html"
                            where p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
    where
      pattern = "/index.html"
      replacement = const "/"

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
  where idx = "index.html"
