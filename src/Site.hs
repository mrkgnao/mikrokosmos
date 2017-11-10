{-# LANGUAGE OverloadedStrings, OverloadedLists, LambdaCase #-}
import           Data.Monoid ((<>))
import           Hakyll
import           Data.List              (sortBy,isSuffixOf)
import           System.FilePath.Posix  (takeBaseName,takeDirectory,(</>))
import Data.Typeable
import Data.Binary
import Hakyll.Contrib.LaTeX
import Image.LaTeX.Render.Pandoc
import Image.LaTeX.Render
import Text.Pandoc.Definition

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

pandocFormulaOptions :: PandocFormulaOptions
pandocFormulaOptions = PandocFormulaOptions
   { shrinkBy = 4
   , errorDisplay = displayError
   , formulaOptions = \case DisplayMath -> displaymath'; _ -> math'
   }

fromPackageList :: [String] -> String
fromPackageList = concatMap (\pkg -> "\\usepackage{" <> pkg <> "}")

packageList :: [String]
packageList = ["amsmath", "amsfonts"]

pkgInv :: String
pkgInv = (fromPackageList packageList) <> 
  (  "\\usepackage[euler-digits,euler-hat-accent]{eulervm}"
  ++ "\\usepackage{palatino}"
  ++ "\\usepackage{tikz}"
  ++ "\\usetikzlibrary{cd}"

  ++ "\\newcommand{\\id}[1]{{\\sf id} _ {#1}}"
  ++ "\\newcommand{\\ob}[1]{{\\sf Ob}\\,#1}"
  ++ "\\newcommand{\\cmor}[3]{{\\sf Mor} _ {#1}(#2, #3)}"
  ++ "\\newcommand{\\cmoc}[2]{{\\sf Mor} _ {\\sf C}(#1, #2)}"
  ++ "\\newcommand{\\mor}[3]{{#1}(#2, #3)}"
  ++ "\\newcommand{\\moc}[2]{{\\sf C}(#1, #2)}"
  ++ "\\newcommand{\\trr}{\\triangleright}"
  ++ "\\newcommand{\\rc}{\\mathsf{C}}"
  ++ "\\newcommand{\\set}{\\mathsf{Set}}"
  ++ "\\DeclareFontFamily{OT1}{slmss}{}"
  ++ "\\DeclareFontShape{OT1}{slmss}{m}{n}"
  ++ "     {<-8.5> s*[1.0] rm-lmss8"
  ++ "      <8.5-9.5> s*[1.0] rm-lmss9"
  ++ "      <9.5-11> s*[1.0] rm-lmss10"
  ++ "      <11-15.5> s*[1.0] rm-lmss12"
  ++ "      <15.5-> s*[1.0] rm-lmss17"
  ++ "     }{}"
  ++ "\\DeclareSymbolFont{sfoperators}{OT1}{slmss}{m}{n}"
  ++ "\\DeclareSymbolFontAlphabet{\\mathsf}{sfoperators}"
  ++ "\\makeatletter"
  ++ "\\def\\operator@font{\\mathgroup\\symsfoperators}"
  ++ "\\makeatother"
  )

defaultInv :: String
defaultInv = (fromPackageList ["amsmath", "amssymb", "amsfonts"])

displaymath' :: FormulaOptions
displaymath' = FormulaOptions pkgInv "displaymath" 600

-- | Use the @amsmath@ package, the @math@ environment, and 200dpi.
math' :: FormulaOptions
math' = FormulaOptions pkgInv "math" 500

main :: IO ()
main = do
  renderFormulae <- initFormulaCompilerDataURI 1000 defaultEnv
  -- let renderFormulae = compileFormulaeDataURI defaultEnv
  hakyll (site renderFormulae)

-- site :: Rules ()
site rf = do
  matchGlob imagesDir "*"     idRouteCopy
  matchGlob cssDir    "*.css" (idRouteWith compressCssCompiler)
  match "css/*.hs" $ routeWith
    (setExtension "css")
    (getResourceString >>= withItemBody (unixFilter "runghc" []))
  match  staticPat        staticPages
  match  "posts/**"       (postRules rf)
  match  "drafts/**"      (postRules rf)
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

-- postRules :: Rules ()
postRules rf = do
  cleanRouteWith
    $   pandocCompilerWithTransformM defaultHakyllReaderOptions
                                     defaultHakyllWriterOptions
                                     (rf pandocFormulaOptions)
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

