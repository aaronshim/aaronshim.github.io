{-# LANGUAGE DeriveGeneric #-}
-- Add this line to enable the \case syntax
{-# LANGUAGE OverloadedStrings #-}

module Csp
  ( -- * Types
    CspOptions (..),
    ScriptInfo (..),

    -- * CSP Generation
    defaultCspOptions,
    getStrictCsp,

    -- * HTML Manipulation & Hashing
    hashInlineScript,
    hashAllInlineScripts,
    addMetaTag,
    refactorSourcedScriptsForHashBasedCsp,
    createLoaderScript,

    -- * Utility
    serializeTags, -- Re-export or utility function
  )
where

-- Explicitly import length

-- Useful for structured search/insertion
-- Import Algorithms qualified
import qualified Crypto.Hash as Hash -- Import main module qualified
import qualified Crypto.Hash.Algorithms as Hash.Algorithms
import Data.Aeson
  ( FromJSON (..),
    Options (..),
    ToJSON (..),
    defaultOptions,
    encode,
    genericParseJSON,
    genericToJSON,
  )
import Data.ByteArray (convert) -- Import the convert function
import qualified Data.ByteString.Base64 as B64
import Data.Char (toLower)
import Data.List (foldl')
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import Text.HTML.TagSoup
  ( Tag (..),
    parseTags,
    renderTags,
  )

-- | Options for generating the CSP string.
data CspOptions = CspOptions
  { -- | Add 'https:' and 'unsafe-inline' for older browsers (ignored by modern ones with strict-dynamic/hashes).
    enableBrowserFallbacks :: Bool,
    -- | Add 'require-trusted-types-for 'script''.
    enableTrustedTypes :: Bool,
    -- | Add 'unsafe-eval' (makes policy less secure).
    enableUnsafeEval :: Bool
  }
  deriving (Show, Eq)

-- | Default options for CSP generation.
defaultCspOptions :: CspOptions
defaultCspOptions =
  CspOptions
    { enableBrowserFallbacks = True,
      enableTrustedTypes = False,
      enableUnsafeEval = False
    }

-- | Information about a sourced script to be loaded dynamically.
data ScriptInfo = ScriptInfo
  { -- | The source URL of the script.
    scriptInfoSrc :: Text,
    -- | The optional type attribute of the script.
    scriptInfoType :: Maybe Text
  }
  deriving (Show, Eq, Generic)

-- Helper function to lowercase the first character of a String
lowercaseFirst :: String -> String
lowercaseFirst [] = []
lowercaseFirst (c : cs) = toLower c : cs

-- Customize field names for JSON compatibility with the JS version
instance ToJSON ScriptInfo where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = lowercaseFirst . drop (length ("scriptInfo" :: String))
        }

instance FromJSON ScriptInfo where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = lowercaseFirst . drop (length ("scriptInfo" :: String))
        }

-- Constants
hashAlgorithmName :: Text
hashAlgorithmName = "sha256"

-- Use the algorithm type from Crypto.Hash.Algorithms
hashAlgorithm :: Hash.Algorithms.SHA256
hashAlgorithm = Hash.Algorithms.SHA256

-- | Calculates a CSP-compatible Base64 encoded hash of script content.
-- Prepends the required "'sha256-...'" prefix.
hashInlineScript :: Text -> Text
hashInlineScript scriptText =
  let scriptBytes = TE.encodeUtf8 scriptText
      -- Use Hash.hashWith from Crypto.Hash and convert from Data.ByteArray
      hashDigest = Hash.hashWith hashAlgorithm scriptBytes :: Hash.Digest Hash.Algorithms.SHA256
      base64Hash = B64.encode (convert hashDigest) -- Use convert function
   in T.concat ["'", hashAlgorithmName, "-", TE.decodeUtf8 base64Hash, "'"]

-- | Finds all inline scripts in HTML Text and returns their CSP hashes.
-- Parses the HTML once and uses a state machine to accurately extract content.
hashAllInlineScripts :: Text -> [Text]
hashAllInlineScripts html =
  extractHashes $ parseTags html
  where
    -- State machine function using foldl'
    -- State: Maybe Text -> Nothing means outside script, Just acc means inside script accumulating Text
    -- Result: [Text] -> Accumulated list of hashes
    extractHashes :: [Tag Text] -> [Text]
    extractHashes tags = reverse $ snd $ foldl' processTag (Nothing, []) tags

    processTag :: (Maybe Text, [Text]) -> Tag Text -> (Maybe Text, [Text])
    processTag (currentState, hashes) tag =
      case currentState of
        -- Currently Outside an inline script tag
        Nothing ->
          case tag of
            -- Found the start of an inline script (no 'src')
            TagOpen "script" attrs
              | not (any (\(k, _) -> k == "src") attrs) ->
                  (Just "", hashes) -- Enter 'InsideScript' state with empty accumulator ""
                  -- Ignore other tags when outside
            _ -> (Nothing, hashes)
        -- Currently Inside an inline script tag, accumulating content
        Just accumulator ->
          case tag of
            -- Found text content, append it to the accumulator
            TagText text ->
              (Just (accumulator <> text), hashes) -- Append text

            -- Found the end of the script tag
            TagClose "script" ->
              let newHash = hashInlineScript accumulator -- Hash the accumulated content
               in (Nothing, newHash : hashes) -- Add hash to list, exit 'InsideScript' state

            -- Found something else inside script (e.g., comment, nested tag - ignored by this simple state machine)
            -- CSP hashing generally includes comments, so a more complex parser might be needed
            -- for perfect accuracy if comments within <script> are expected.
            _ -> (Just accumulator, hashes) -- Stay inside, keep accumulating (or ignore tag)

-- | Generates the Content Security Policy string based on options and script hashes.
getStrictCsp :: Maybe [Text] -> CspOptions -> Text
getStrictCsp maybeHashes options =
  let hashes = fromMaybe [] maybeHashes
      basePolicy =
        [ ("script-src", "'strict-dynamic'" : hashes),
          ("object-src", ["'none'"]),
          ("base-uri", ["'self'"]) -- Use 'self' instead of 'none' if relative URLs are needed
        ]

      policyWithFallbacks =
        if enableBrowserFallbacks options
          then
            let scriptSrcUpdate =
                  if not (null hashes)
                    then ["https:", "'unsafe-inline'"]
                    else ["https:"]
             in updateDirective "script-src" (++ scriptSrcUpdate) basePolicy
          else basePolicy

      policyWithTrustedTypes =
        if enableTrustedTypes options
          then policyWithFallbacks ++ [("require-trusted-types-for", ["'script'"])]
          else policyWithFallbacks

      finalPolicy =
        if enableUnsafeEval options
          then updateDirective "script-src" (++ ["'unsafe-eval'"]) policyWithTrustedTypes
          else policyWithTrustedTypes

      formatDirective (name, values) = T.concat [name, " ", T.intercalate " " values, ";"]
   in T.intercalate "" $ map formatDirective finalPolicy

-- Helper to update or add a directive in the policy list
updateDirective :: Text -> ([Text] -> [Text]) -> [(Text, [Text])] -> [(Text, [Text])]
updateDirective key modifyFn policy =
  case lookup key policy of
    Just _ -> map (\(k, v) -> if k == key then (k, modifyFn v) else (k, v)) policy
    Nothing -> policy -- Or add if needed: policy ++ [(key, modifyFn [])]

-- | Adds a CSP meta tag to the HTML <head>. Replaces existing CSP meta tag if found.
-- Also replaces escaped single quotes (&#39;) with literal quotes (') in the CSP string
-- for better readability, as browsers handle them correctly in double-quoted attributes.
addMetaTag :: Text -> Text -> Text
addMetaTag cspValue html =
  let tags = parseTags html
      -- The cspValue itself contains the literal single quotes required by CSP syntax
      cspMetaTag = TagOpen "meta" [("http-equiv", "Content-Security-Policy"), ("content", cspValue)]
      -- Remove existing CSP meta tags
      tagsWithoutOldCsp = filter (not . isCspMetaTag) tags
      -- Find head section using helper function
      (beforeHead, maybeHeadAndRest) = break isHeadOpenTag tagsWithoutOldCsp

      -- Render the tags using TagSoup
      renderedHtmlWithEscapedQuotes = serializeTags $ case maybeHeadAndRest of
        -- Head tag found: insert meta tag right after <head>
        (headOpen : restOfHead) -> beforeHead ++ [headOpen, cspMetaTag] ++ restOfHead
        -- No head tag found: prepend meta tag to the beginning (less ideal but functional)
        [] -> cspMetaTag : tagsWithoutOldCsp

      -- Replace the escaped single quotes specifically generated by TagSoup within the CSP content
      -- This is safe because the CSP value is placed in a double-quoted attribute.
      finalHtml = T.replace "&#39;" "'" renderedHtmlWithEscapedQuotes
   in finalHtml

-- Check if a tag is <meta http-equiv="Content-Security-Policy" ...>
isCspMetaTag :: Tag Text -> Bool
isCspMetaTag (TagOpen "meta" attrs) = any (\(k, v) -> T.toLower k == "http-equiv" && T.toLower v == "content-security-policy") attrs
isCspMetaTag _ = False

-- Helper function to check for opening <head> tag
isHeadOpenTag :: Tag Text -> Bool
isHeadOpenTag (TagOpen "head" _) = True
isHeadOpenTag _ = False

-- Helper function to remove sourced script blocks (open tag + corresponding close tag)
-- It maintains a counter of sourced script opening tags encountered,
-- and skips a closing tag for each count > 0.
processTagsForRemoval :: [Tag Text] -> [Tag Text]
processTagsForRemoval tags = reverse $ fst $ foldl' step ([], 0) tags
  where
    step :: ([Tag Text], Int) -> Tag Text -> ([Tag Text], Int)
    step (accum, skipClosingCount) tag =
      case tag of
        (TagOpen "script" attrs)
          | any (\(k, _) -> k == "src") attrs ->
              -- Found sourced script opening tag: skip it, increment counter
              (accum, skipClosingCount + 1)
        (TagClose "script")
          | skipClosingCount > 0 ->
              -- Found a closing script tag while counter > 0: skip it, decrement counter
              (accum, skipClosingCount - 1)
        _ ->
          -- Any other tag, or a closing script tag when counter is 0: keep it
          (tag : accum, skipClosingCount)

-- | Refactors sourced scripts (<script src="...">) into a single inline loader script.
-- Returns the modified HTML Text.
refactorSourcedScriptsForHashBasedCsp :: Text -> Text
refactorSourcedScriptsForHashBasedCsp html =
  let allTags = parseTags html
      -- Extract info needed for the loader script *before* removing tags
      sourcedScriptsForInfo = filter isSourcedScript allTags
      scriptInfos :: [ScriptInfo]
      scriptInfos = mapMaybe extractScriptInfo sourcedScriptsForInfo

      -- Create the loader script content if needed
      maybeLoaderScriptContent :: Maybe Text
      maybeLoaderScriptContent = createLoaderScript scriptInfos
   in case maybeLoaderScriptContent of
        Nothing -> html -- No sourced scripts, return original HTML
        Just loaderContent ->
          let -- Create the loader script tags structure
              loaderTag = TagOpen "script" []
              loaderText = TagText loaderContent
              loaderCloseTag = TagClose "script"
              loaderScriptTags = [loaderTag, loaderText, loaderCloseTag]

              -- Process the original tags to remove sourced script blocks
              tagsWithSourcedRemoved = processTagsForRemoval allTags

              -- Try to insert the loader script before </body>
              (beforeBodyEnd, maybeBodyEndAndRest) = break isBodyCloseTag tagsWithSourcedRemoved
           in serializeTags $ case maybeBodyEndAndRest of
                (bodyEndTag : afterBodyEnd) -> beforeBodyEnd ++ loaderScriptTags ++ [bodyEndTag] ++ afterBodyEnd
                -- No </body> found, append to the end
                [] -> tagsWithSourcedRemoved ++ loaderScriptTags

-- Check if a tag is <script> with a 'src' attribute
isSourcedScript :: Tag Text -> Bool
isSourcedScript (TagOpen "script" attrs) = any (\(k, _) -> k == "src") attrs
isSourcedScript _ = False

-- Extract ScriptInfo from a sourced script tag
extractScriptInfo :: Tag Text -> Maybe ScriptInfo
extractScriptInfo (TagOpen "script" attrs) =
  case lookup "src" attrs of
    Just src -> Just $ ScriptInfo {scriptInfoSrc = src, scriptInfoType = lookup "type" attrs}
    Nothing -> Nothing
extractScriptInfo _ = Nothing

-- Helper function to check for closing </body> tag
isBodyCloseTag :: Tag Text -> Bool
isBodyCloseTag (TagClose "body") = True
isBodyCloseTag _ = False

-- | Creates the JavaScript loader script content.
createLoaderScript :: [ScriptInfo] -> Maybe Text
createLoaderScript [] = Nothing
createLoaderScript scriptInfoList =
  Just $
    T.concat
      [ "\nvar scripts = ",
        TL.toStrict $ TLE.decodeUtf8 $ encode scriptInfoList, -- Encode script info list to JSON
        ";\n",
        "scripts.forEach(function(scriptInfo) {\n",
        "  var s = document.createElement('script');\n",
        "  s.src = scriptInfo.src;\n",
        "  if (scriptInfo.type) {\n",
        "    s.type = scriptInfo.type;\n",
        "  }\n",
        "  s.async = false; // preserve execution order.\n", -- Corrected JS comment syntax
        "  document.body.appendChild(s);\n",
        "});\n"
      ]

-- | Utility to serialize tags back to Text.
serializeTags :: [Tag Text] -> Text
serializeTags = renderTags
