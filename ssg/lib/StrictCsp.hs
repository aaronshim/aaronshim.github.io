module StrictCsp (applyStrictCsp, applyStrictCspWithDefaultOptions) where

import Csp -- Import your module
import qualified Data.Text as T

-- import Debug.Trace -- Removed or comment out trace if not needed

-- | Applies strict CSP processing to HTML Text, mimicking the JS example flow.
--
-- Takes HTML as input and returns HTML with:
-- 1. Sourced scripts refactored into a single inline loader (respecting Trusted Types option).
-- 2. A CSP meta tag added, including hashes of all inline scripts (including the new loader).
-- Updated type signature to accept CspOptions
applyStrictCsp :: CspOptions -> T.Text -> T.Text
applyStrictCsp cspOptions initialHtml =
  -- 1. Refactor sourced scripts, passing options
  let refactoredHtml = refactorSourcedScriptsForHashBasedCsp cspOptions initialHtml -- Pass options

      -- 2. Hash inline scripts from the *refactored* html (including the loader script)
      scriptHashes = hashAllInlineScripts refactoredHtml

      -- 3. Generate a strict CSP string using the collected hashes and options
      strictCspString = getStrictCsp (Just scriptHashes) cspOptions

      -- 4. Set this CSP via a meta tag in the *refactored* html
      htmlWithCsp = addMetaTag strictCspString refactoredHtml
   in -- 5. Return the final HTML string
      htmlWithCsp

-- | Applies strict CSP processing using default options.
applyStrictCspWithDefaultOptions :: T.Text -> T.Text
applyStrictCspWithDefaultOptions = applyStrictCsp defaultCspOptions -- This definition remains correct
