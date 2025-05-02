module StrictCsp (applyStrictCsp, applyStrictCspWithDefaultOptions) where

-- For main example
import Csp -- Import your module
import qualified Data.Text as T
import Debug.Trace

-- \| Applies strict CSP processing to HTML Text, mimicking the JS example flow.
--
-- Takes HTML as input and returns HTML with:
-- 1. Sourced scripts refactored into a single inline loader.
-- 2. A CSP meta tag added, including hashes of all inline scripts (including the new loader).
applyStrictCsp :: CspOptions -> T.Text -> T.Text
applyStrictCsp cspOptions initialHtml =
  -- 1. Refactor sourced scripts so that we can set a strict hash-based CSP
  let refactoredHtml = trace "refactorSourcedScriptsForHashBasedCsp" $ refactorSourcedScriptsForHashBasedCsp initialHtml

      -- 2. Hash inline scripts from the *refactored* html (including the loader script)
      scriptHashes = trace "hashAllInlineScripts" $ hashAllInlineScripts refactoredHtml

      -- 3. Generate a strict CSP string using the collected hashes and options
      strictCspString = trace "getStrictCsp" $ getStrictCsp (Just scriptHashes) cspOptions

      -- 4. Set this CSP via a meta tag in the *refactored* html
      htmlWithCsp = trace "addMetaTag" $ addMetaTag strictCspString refactoredHtml
   in -- 5. Return the final HTML string
      htmlWithCsp

applyStrictCspWithDefaultOptions :: T.Text -> T.Text
applyStrictCspWithDefaultOptions = applyStrictCsp defaultCspOptions