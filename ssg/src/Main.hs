{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Csp (CspOptions (CspOptions, enableBrowserFallbacks, enableTrustedTypes, enableUnsafeEval))
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Slugger as Slugger
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime)
import GhcVersionResolver (ghcVersionContext)
import Hakyll
import PandocCollapsible (makeCollapsible)
import qualified StrictCsp
import System.FilePath (takeBaseName, takeFileName, (</>))
import Text.Pandoc
  ( Extension (Ext_fenced_code_attributes, Ext_footnotes, Ext_gfm_auto_identifiers, Ext_implicit_header_references, Ext_smart),
    Extensions,
    ReaderOptions,
    WriterOptions (writerHighlightStyle),
    extensionsFromList,
    githubMarkdownExtensions,
    readerExtensions,
    writerExtensions,
  )
import Text.Pandoc.Highlighting (Style, breezeDark, styleToCss)

--------------------------------------------------------------------------------
-- PERSONALIZATION

mySiteName :: String
mySiteName = "Aaron Shim"

mySiteRoot :: String
mySiteRoot = "https:/aaronshim.github.io"

myFeedTitle :: String
myFeedTitle = "My Feed Title"

myFeedDescription :: String
myFeedDescription = "My Site Description"

myFeedAuthorName :: String
myFeedAuthorName = "Aaron Shim"

myFeedAuthorEmail :: String
myFeedAuthorEmail = "me@myemail.com"

myFeedRoot :: String
myFeedRoot = mySiteRoot

--------------------------------------------------------------------------------
-- CONFIG

expandedHeadingLevel :: Int
expandedHeadingLevel = 2

-- Default configuration: https://github.com/jaspervdj/hakyll/blob/cd74877d41f41c4fba27768f84255e797748a31a/lib/Hakyll/Core/Configuration.hs#L101-L125
config :: Configuration
config =
  defaultConfiguration
    { destinationDirectory = "dist",
      ignoreFile = ignoreFile',
      previewHost = "127.0.0.1",
      previewPort = 8000,
      providerDirectory = "src",
      storeDirectory = "ssg/_cache",
      tmpDirectory = "ssg/_tmp"
    }
  where
    ignoreFile' path
      | "." `isPrefixOf` fileName = False
      | "#" `isPrefixOf` fileName = True
      | "~" `isSuffixOf` fileName = True
      | ".swp" `isSuffixOf` fileName = True
      | otherwise = False
      where
        fileName = takeFileName path

--------------------------------------------------------------------------------
-- BUILD

main :: IO ()
main = do
  putStrLn "Hello, world!"

  hakyllWith config $ do
    forM_
      [ "CNAME",
        "favicon.ico",
        "robots.txt",
        "_config.yml",
        "images/*",
        "js/*",
        "fonts/*"
      ]
      $ \f -> match f $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
      route idRoute
      compile compressCssCompiler

    match (fromList ["work.md", "music.md", "projects.md"]) $ do
      route $ customRoute $ \ident ->
        takeBaseName (toFilePath ident) </> "index.html"
      compile $ do
        let ctx = siteContext
        collapsiblePandocCompiler expandedHeadingLevel
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= applyDefaultCsp

    match "posts/*" $ do
      let ctx = constField "type" "article" <> postCtx

      route $ metadataRoute postRoute
      compile $
        pandocCompilerCustom
          >>= loadAndApplyTemplate "templates/post.html" ctx
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= applyDefaultCsp

    match "index.html" $ do
      route idRoute
      -- Not every post should be included in the index page.
      compile $ buildIndexLikePage (take 5)

    match "posts.html" $ do
      route $ constRoute "posts/index.html"
      compile $ buildIndexLikePage id

    match "templates/*" $
      compile templateBodyCompiler

    create ["sitemap.xml"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"

        let pages = posts
            sitemapCtx =
              constField "root" mySiteRoot
                <> constField "siteName" mySiteName
                <> listField "pages" postCtx (return pages)

        makeItem ("" :: String)
          >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

    create ["rss.xml"] $ do
      route idRoute
      compile (feedCompiler renderRss)

    create ["atom.xml"] $ do
      route idRoute
      compile (feedCompiler renderAtom)

    create ["css/code.css"] $ do
      route idRoute
      compile (makeStyle pandocHighlightStyle)

-- | A helper function to build pages that list posts, like the homepage or archive page.
-- It takes a function that selects a subset of posts to display.
buildIndexLikePage ::
  ([Item String] -> [Item String]) -> -- Function to select posts
  Compiler (Item String) -- Resulting compiler action
buildIndexLikePage selectPosts = do
  -- Load and sort all posts
  allPostsSorted <- recentFirst =<< loadAll "posts/*"
  -- Apply the selection function
  let postsToUse = selectPosts allPostsSorted

  -- Define the context using the selected posts
  let ctx =
        listField "posts" postCtx (return postsToUse) <> siteContext

  -- Run the common compilation pipeline
  getResourceBody
    >>= applyAsTemplate ctx
    >>= loadAndApplyTemplate "templates/default.html" ctx
    >>= applyDefaultCsp

--------------------------------------------------------------------------------
-- COMPILER HELPERS

-- | Creates a CSS item from a Pandoc style, and compresses it.
makeStyle :: Style -> Compiler (Item String)
makeStyle =
  makeItem . compressCss . styleToCss

-- | A helper to apply a Text transformation to an Item's body.
applyHtmlTextTransform :: (T.Text -> T.Text) -> Item String -> Compiler (Item String)
applyHtmlTextTransform textTransform item =
  return $ fmap (T.unpack . textTransform . T.pack) item

cspSettings :: CspOptions
cspSettings =
  CspOptions
    { enableBrowserFallbacks = True,
      enableTrustedTypes = True,
      enableUnsafeEval = False
    }

-- | Applies a strict Content-Security-Policy to an HTML item.
applyDefaultCsp :: Item String -> Compiler (Item String)
applyDefaultCsp = applyHtmlTextTransform (StrictCsp.applyStrictCsp cspSettings)

--------------------------------------------------------------------------------
-- CONTEXT

-- | The context for feed entries. Used for RSS and Atom feeds.
feedCtx :: Context String
feedCtx =
  titleCtx
    <> postCtx
    <> bodyField "description"

postCtx :: Context String
postCtx =
  dateField "date" "%d %b %Y"
    <> dateField "datetime" (iso8601DateFormat Nothing)
    <> siteContext

-- | Fields that are required by the default.html template.
--   These are expected to be present in the metadata of each page.
--   This context provides fallback values to prevent build failures.
requiredDefaultFieldsCtx :: Context String
requiredDefaultFieldsCtx =
  field "title" (fmap (fromMaybe "Untitled" . lookupString "title") . getMetadata . itemIdentifier)
    <> field "description" (fmap (fromMaybe "No description provided." . lookupString "description") . getMetadata . itemIdentifier)
    <> field "lang" (fmap (fromMaybe "en-us" . lookupString "lang") . getMetadata . itemIdentifier)

-- | Context with site-wide variables for the default template.
siteContext :: Context String
siteContext =
  constField "siteName" mySiteName
    <> constField "feedTitle" myFeedTitle
    <> constField "root" mySiteRoot
    <> requiredDefaultFieldsCtx
    <> copyrightCtx
    <> ghcVersionContext
    <> defaultContext

-- | Context for the page title. Handles HTML ampersand encoding.
titleCtx :: Context String
titleCtx =
  field "title" updatedTitle

-- | Year that the site is built, uses unsafe IO.
yearCtx :: Context String
yearCtx = field "year" $ \_ -> unsafeCompiler $ do
  -- Use \_ to explicitly ignore the Item argument
  currentTime <- liftIO getCurrentTime
  timeZone <- liftIO getCurrentTimeZone
  let localTime = utcToLocalTime timeZone currentTime
  let currentYear = formatTime defaultTimeLocale "%Y" localTime
  return currentYear

-- | Copyright information -- name and year.
copyrightCtx :: Context String
copyrightCtx =
  constField "authorName" myFeedAuthorName
    <> yearCtx

--------------------------------------------------------------------------------
-- TITLE HELPERS

replaceAmp :: String -> String
replaceAmp =
  replaceAll "&" (const "&amp;")

replaceTitleAmp :: Metadata -> String
replaceTitleAmp =
  replaceAmp . safeTitle

safeTitle :: Metadata -> String
safeTitle =
  fromMaybe "no title" . lookupString "title"

updatedTitle :: Item a -> Compiler String
updatedTitle =
  fmap replaceTitleAmp . getMetadata . itemIdentifier

--------------------------------------------------------------------------------
-- PANDOC

-- | A custom Pandoc compiler that uses our specific extensions and options.
pandocCompilerCustom :: Compiler (Item String)
pandocCompilerCustom =
  pandocCompilerWith pandocReaderOpts pandocWriterOpts

pandocExtensionsCustom :: Extensions
pandocExtensionsCustom =
  githubMarkdownExtensions
    <> extensionsFromList
      [ Ext_fenced_code_attributes,
        Ext_gfm_auto_identifiers,
        Ext_implicit_header_references,
        Ext_smart,
        Ext_footnotes
      ]

pandocReaderOpts :: ReaderOptions
pandocReaderOpts =
  defaultHakyllReaderOptions
    { readerExtensions = pandocExtensionsCustom
    }

pandocWriterOpts :: WriterOptions
pandocWriterOpts =
  defaultHakyllWriterOptions
    { writerExtensions = pandocExtensionsCustom,
      writerHighlightStyle = Just pandocHighlightStyle
    }

pandocHighlightStyle :: Style
pandocHighlightStyle =
  breezeDark -- https://hackage.haskell.org/package/pandoc/docs/Text-Pandoc-Highlighting.html

collapsiblePandocCompiler :: Int -> Compiler (Item String)
collapsiblePandocCompiler expandedLevel =
  fmap (writePandocWith pandocWriterOpts . fmap (makeCollapsible expandedLevel)) $
    getResourceBody >>= readPandocWith pandocReaderOpts

--------------------------------------------------------------------------------
-- FEEDS

-- | A type alias for Hakyll's feed rendering functions.
type FeedRenderer =
  FeedConfiguration ->
  Context String ->
  [Item String] ->
  Compiler (Item String)

-- | A helper to compile a feed with a given renderer (RSS or Atom).
feedCompiler :: FeedRenderer -> Compiler (Item String)
feedCompiler renderer =
  renderer feedConfiguration feedCtx
    =<< recentFirst
    =<< loadAllSnapshots "posts/*" "content"

feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
    { feedTitle = myFeedTitle,
      feedDescription = myFeedDescription,
      feedAuthorName = myFeedAuthorName,
      feedAuthorEmail = myFeedAuthorEmail,
      feedRoot = myFeedRoot
    }

--------------------------------------------------------------------------------
-- CUSTOM ROUTE

-- | Creates a URL-friendly slug from a page's title metadata.
fileNameFromTitle :: Metadata -> FilePath
fileNameFromTitle =
  T.unpack . (`T.append` ".html") . Slugger.toSlug . T.pack . safeTitle

-- | Creates a route for a post based on its title, e.g., "posts/hello-world.html".
postRoute :: Metadata -> Routes
postRoute meta =
  let baseFilename = fileNameFromTitle meta -- e.g., "hello-world.html"
      finalPath = "posts" </> baseFilename -- e.g., "posts/hello-world.html"
   in constRoute finalPath -- Create route for the full path
