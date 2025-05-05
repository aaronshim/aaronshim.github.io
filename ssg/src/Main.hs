{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Slugger as Slugger
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime)
import Hakyll
import qualified StrictCsp
import System.FilePath (takeFileName, (</>))
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
mySiteName = "My Site Name"

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

    match "posts/*" $ do
      let ctx = constField "type" "article" <> postCtx

      route $ metadataRoute postRoute
      compile $
        pandocCompilerCustom
          >>= loadAndApplyTemplate "templates/post.html" ctx
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= applyDefaultCsp

    -- Helper function encapsulating the common logic for index-like pages
    let buildIndexLikePage ::
          ([Item String] -> [Item String]) -> -- Function to select posts
          Compiler (Item String) -- Resulting compiler action
        buildIndexLikePage selectPosts = do
          -- Load and sort all posts
          allPostsSorted <- recentFirst =<< loadAll "posts/*"
          -- Apply the selection function
          let postsToUse = selectPosts allPostsSorted

          -- Define the context using the selected posts
          -- It can access postCtx, mySiteRoot, etc. from the outer scope
          let ctx =
                listField "posts" postCtx (return postsToUse)
                  <> constField "root" mySiteRoot
                  <> constField "feedTitle" myFeedTitle
                  <> constField "siteName" mySiteName
                  <> copyrightCtx
                  <> defaultContext

          -- Run the common compilation pipeline
          getResourceBody
            >>= applyAsTemplate ctx
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

--------------------------------------------------------------------------------
-- COMPILER HELPERS

makeStyle :: Style -> Compiler (Item String)
makeStyle =
  makeItem . compressCss . styleToCss

applyHtmlTextTransform :: (T.Text -> T.Text) -> Item String -> Compiler (Item String)
applyHtmlTextTransform textTransform item =
  return $ fmap (T.unpack . textTransform . T.pack) item

applyDefaultCsp :: Item String -> Compiler (Item String)
applyDefaultCsp = applyHtmlTextTransform (StrictCsp.applyStrictCspWithDefaultOptions)

--------------------------------------------------------------------------------
-- CONTEXT

feedCtx :: Context String
feedCtx =
  titleCtx
    <> postCtx
    <> bodyField "description"

postCtx :: Context String
postCtx =
  constField "root" mySiteRoot
    <> constField "feedTitle" myFeedTitle
    <> constField "siteName" mySiteName
    <> field "description" (fmap (fromMaybe "" . lookupString "description") . getMetadata . itemIdentifier)
    <> dateField "date" "%d %b %Y"
    <> dateField "datetime" (iso8601DateFormat Nothing)
    <> copyrightCtx
    <> defaultContext

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

--------------------------------------------------------------------------------
-- FEEDS

type FeedRenderer =
  FeedConfiguration ->
  Context String ->
  [Item String] ->
  Compiler (Item String)

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

fileNameFromTitle :: Metadata -> FilePath
fileNameFromTitle =
  T.unpack . (`T.append` ".html") . Slugger.toSlug . T.pack . safeTitle

-- | For posts, the generated filename is based on the title and lives under \/posts\/
postRoute :: Metadata -> Routes
postRoute meta =
  let baseFilename = fileNameFromTitle meta -- e.g., "hello-world.html"
      finalPath = "posts" </> baseFilename -- e.g., "posts/hello-world.html"
   in constRoute finalPath -- Create route for the full path
