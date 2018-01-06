{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Binary
import           Data.Default
import qualified Data.HashMap.Strict          as M
import           Data.List                    (isSuffixOf, sortBy)
import           Data.Maybe
import           Data.Monoid                  ((<>))
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import           Data.Typeable
import           Hakyll
import           Hakyll.Contrib.LaTeX
import           Hakyll.Core.Compiler
import qualified System.FilePath              as FilePath
import           System.FilePath.Posix        (takeBaseName, takeDirectory,
                                               (</>))
import           Text.Pandoc.Definition

import qualified Language.Haskell.Interpreter as LHI
import qualified Text.Pandoc                  as TP
import           Text.Pandoc.Walk             (walkM)

say :: String -> LHI.Interpreter ()
say = LHI.liftIO . putStrLn

doEval :: TP.Block -> LHI.Interpreter TP.Block
doEval cb@(TP.CodeBlock (id, classes, namevals) contents) = do
  r <- LHI.eval contents
  let contents' = (unlines $ map ("λ> " ++) $ lines contents) ++ "" ++ r
  return $ TP.CodeBlock (id, classes, namevals) contents'

evalBlock :: TP.Block -> LHI.Interpreter TP.Block
evalBlock cb@(TP.CodeBlock (id, classes, namevals) contents) =
  if "haskell-eval" `notElem` classes
    then pure cb
    else doEval (TP.CodeBlock (id, classes ++ ["haskell"], namevals) contents)
evalBlock x = do
  return x

evalDoc :: [String] -> TP.Pandoc -> Compiler TP.Pandoc
evalDoc modules d =
  unsafeCompiler $ do
    res <-
      LHI.runInterpreter $
      -- force modules to be interpreted (otherwise using this file on itself
      -- fails)
      -- http://stackoverflow.com/questions/7134520/why-cannot-top-level-module-be-set-to-main-in-hint
       do
        let imports = ["*" ++ m | m <- modules]
        LHI.loadModules imports
        xs <- LHI.getLoadedModules
      -- set them as top-level modules so that all of their functions (not only
      -- the ones exported) can be accessed
        LHI.setTopLevelModules xs
        LHI.setImports ["Prelude"]
        walkM evalBlock d
    case res of
      Left (LHI.WontCompile xs) -> error (show xs)
      Left err                  -> error (show err)
      Right r                   -> pure r

imagesDir = "images"

assetsDir = "assets"

cssDir = "css"

postsDir = "posts"

draftsDir = "drafts"

templatesDir = "templates"

matchGlob root glob = match (fromGlob (root </> glob))

staticPat :: Pattern
staticPat = fromList ["about.md", "things-i-like.md", "contact.markdown"]

routeWith ::
     (Writable a, Typeable a, Binary a)
  => Routes
  -> Compiler (Item a)
  -> Rules ()
routeWith rt cpr = route rt *> compile cpr

idRouteWith ::
     (Writable a, Typeable a, Binary a) => Compiler (Item a) -> Rules ()
idRouteWith = routeWith idRoute

cleanRouteWith ::
     (Writable a, Typeable a, Binary a) => Compiler (Item a) -> Rules ()
cleanRouteWith = routeWith cleanRoute

idRouteCopy :: Rules ()
idRouteCopy = idRouteWith copyFileCompiler

main :: IO ()
main = do
  hakyll site

-- site :: Rules ()
site = do
  matchGlob imagesDir "*" idRouteCopy
  matchGlob assetsDir "*" idRouteCopy
  matchGlob cssDir "*.css" (idRouteWith compressCssCompiler)
  match "css/*.hs" $
    routeWith
      (setExtension "css")
      (getResourceString >>= withItemBody (unixFilter "runghc" []))
  match staticPat staticPages
  match "posts/**" postRules
  match "drafts/**" postRules
  create ["archive.html"] archivePage
  match "index.html" indexPage
  matchGlob templatesDir "*.html" (compile templateBodyCompiler)

staticPages =
  cleanRouteWith $
  pandocCompiler >>=
  loadAndApplyTemplate "templates/Default.html" defaultContext >>=
  relativizeUrls

archivePage =
  idRouteWith $ do
    let skipIfEmpty d t =
          if null d
            then mempty
            else listField t postCtx (pure d)
    posts <- loadAll "posts/*" >>= recentFirst
    drafts <- loadAll "drafts/*" >>= recentFirst
    let archiveCtx =
          skipIfEmpty posts "posts" <> skipIfEmpty drafts "drafts" <>
          constField "title" "Archives" <>
          defaultContext
    makeItem "" >>= loadAndApplyTemplate "templates/Archive.html" archiveCtx >>=
      loadAndApplyTemplate "templates/Default.html" archiveCtx >>=
      relativizeUrls

indexPage =
  idRouteWith $ do
    let skipIfEmpty d t =
          if null d
            then mempty
            else listField t postCtx (pure d)
    posts <- loadAll "posts/*" >>= recentFirst
    drafts <- loadAll "drafts/*" >>= recentFirst
    let indexCtx =
          skipIfEmpty posts "posts" <> skipIfEmpty drafts "drafts" <>
          constField "title" "Home" <>
          defaultContext
    getResourceBody >>= applyAsTemplate indexCtx >>=
      loadAndApplyTemplate "templates/Default.html" postCtx >>=
      relativizeUrls >>=
      cleanIndexUrls >>=
      cleanIndexHtmls

-- postRules :: Rules ()
postRules =
  cleanRouteWith $ do
    p <- getResourceFilePath
    let f =
          case FilePath.takeExtension p of
            ".lhs" ->
              pandocCompilerWithTransformM
                defaultHakyllReaderOptions
                (defaultHakyllWriterOptions { TP.writerExtensions = TP.enableExtension TP.Ext_literate_haskell (TP.writerExtensions defaultHakyllWriterOptions) })
                (evalDoc [p])
            _ -> pandocCompiler
    f >>= loadAndApplyTemplate "templates/Post.html" postCtx >>=
      loadAndApplyTemplate "templates/Default.html" postCtx >>=
      relativizeUrls

--------------------------------------------------------------------------------
-- preambleContext :: Context a
-- preambleContext = field "preamble" $ \item -> do
--     metadata <- getMetadata (itemIdentifier item)
--     return $ fromMaybe "No preamble" $ _ $ M.lookup (T.pack "preamble") metadata
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
      where
        p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pat replacement)
  where
    pat = "/index.html"
    replacement = const "/"

cleanIndex :: String -> String
cleanIndex url
  | idx `isSuffixOf` url = take (length url - length idx) url
  | otherwise = url
  where
    idx = "index.html"
-- match "posts/*" $ version "raw" $ do
--     route idRoute
--     compile copyFileCompiler
