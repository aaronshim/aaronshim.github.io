{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Csp -- Import functions from your module
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Csp" $ do

    describe "hashInlineScript" $ do
      it "hashes simple script correctly" $
        hashInlineScript "alert('hello');" `shouldBe` "'sha256-kf+QHCKEpFy7CANBeKjIsydaJfIWxmySPMy/Jj8ULQE='"

      it "hashes script with whitespace correctly" $
        hashInlineScript " alert('hello'); " `shouldBe` "'sha256-aLhWhEAnKvNvAlkGqCmIx4T27j3DRBEsJYWOkS+NhIc='" -- Note: Hash changes due to whitespace

      it "hashes empty script correctly" $
        hashInlineScript "" `shouldBe` "'sha256-47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU='"

    describe "createLoaderScript" $ do
      let script1 = ScriptInfo "script1.js" Nothing
      let script2 = ScriptInfo "script2.js" (Just "module")

      it "returns Nothing for empty list" $
        createLoaderScript [] `shouldBe` Nothing

      it "creates loader for one script" $
        let expected = Just $ T.pack $ "\nvar scripts = [{\"src\":\"script1.js\"}];\n" ++ loaderBoilerplate
        in createLoaderScript [script1] `shouldBe` expected

      it "creates loader for multiple scripts with type" $
         let expected = Just $ T.pack $ "\nvar scripts = [{\"src\":\"script1.js\"},{\"type\":\"module\",\"src\":\"script2.js\"}];\n" ++ loaderBoilerplate
         in createLoaderScript [script1, script2] `shouldBe` expected
        where
          loaderBoilerplate =
            "scripts.forEach(function(scriptInfo) {\n" ++
            "  var s = document.createElement('script');\n" ++
            "  s.src = scriptInfo.src;\n" ++
            "  if (scriptInfo.type) {\n" ++
            "    s.type = scriptInfo.type;\n" ++
            "  }\n" ++
            "  s.async = false; // preserve execution order.\n" ++
            "  document.body.appendChild(s);\n" ++
            "});\n"

    describe "getStrictCsp" $ do
      let hash1 = "'sha256-hash1'"
      let hash2 = "'sha256-hash2'"
      let defaultOpts = defaultCspOptions
      let noFallbackOpts = defaultOpts { enableBrowserFallbacks = False }
      let trustedTypesOpts = defaultOpts { enableTrustedTypes = True }
      let unsafeEvalOpts = defaultOpts { enableUnsafeEval = True }

      it "generates default CSP with no hashes" $
        getStrictCsp Nothing defaultOpts `shouldBe`
          "script-src 'strict-dynamic' https:;object-src 'none';base-uri 'self';"

      it "generates default CSP with hashes" $
        getStrictCsp (Just [hash1, hash2]) defaultOpts `shouldBe`
          "script-src 'strict-dynamic' 'sha256-hash1' 'sha256-hash2' https: 'unsafe-inline';object-src 'none';base-uri 'self';"

      it "generates CSP without fallbacks" $
         getStrictCsp (Just [hash1]) noFallbackOpts `shouldBe`
           "script-src 'strict-dynamic' 'sha256-hash1';object-src 'none';base-uri 'self';"

      it "generates CSP with trusted types" $
         getStrictCsp (Just [hash1]) trustedTypesOpts `shouldBe`
           "script-src 'strict-dynamic' 'sha256-hash1' https: 'unsafe-inline';object-src 'none';base-uri 'self';require-trusted-types-for 'script';"

      it "generates CSP with unsafe-eval" $
         getStrictCsp (Just [hash1]) unsafeEvalOpts `shouldBe`
           "script-src 'strict-dynamic' 'sha256-hash1' https: 'unsafe-inline' 'unsafe-eval';object-src 'none';base-uri 'self';"

    describe "HTML Manipulation" $ do
        let sampleHtml = "<html><head><title>Test</title></head><body><p>Hello</p><script>alert('inline');</script><script src=\"external.js\"></script></body></html>"
        let sampleHtmlNoHead = "<html><body><p>Hello</p><script>alert('inline');</script><script src=\"external.js\"></script></body></html>"
        let sampleHtmlNoBodyEnd = "<html><head></head><body><p>Hello</p><script>alert('inline');</script><script src=\"external.js\"></script>"

        describe "hashAllInlineScripts" $ do
             it "finds and hashes the inline script" $
                 -- Need to adjust expected hash based on actual whitespace/content extraction
                 let expectedHash = hashInlineScript "alert('inline');"
                 in hashAllInlineScripts sampleHtml `shouldBe` [expectedHash]

             it "returns empty list if no inline scripts" $
                 hashAllInlineScripts "<p>Test</p><script src='a.js'></script>" `shouldBe` []

        describe "addMetaTag" $ do
            let csp = "default-src 'self'"
            it "adds meta tag inside head" $
                let expected = "<html><head><meta http-equiv=\"Content-Security-Policy\" content=\"default-src 'self'\"><title>Test</title></head><body><p>Hello</p><script>alert('inline');</script><script src=\"external.js\"></script></body></html>"
                in addMetaTag csp sampleHtml `shouldBe` expected
            it "replaces existing meta tag" $
                let htmlWithMeta = "<html><head><meta http-equiv=\"Content-Security-Policy\" content=\"old-csp\"><title>Test</title></head><body></body></html>"
                let expected = "<html><head><meta http-equiv=\"Content-Security-Policy\" content=\"default-src 'self'\"><title>Test</title></head><body></body></html>"
                in addMetaTag csp htmlWithMeta `shouldBe` expected
            it "prepends meta tag if no head" $
                 let expected = "<meta http-equiv=\"Content-Security-Policy\" content=\"default-src 'self'\"><html><body><p>Hello</p><script>alert('inline');</script><script src=\"external.js\"></script></body></html>"
                 in addMetaTag csp sampleHtmlNoHead `shouldBe` expected


        describe "refactorSourcedScriptsForHashBasedCsp" $ do
            it "removes sourced script and adds loader script before </body>" $
                let result = refactorSourcedScriptsForHashBasedCsp sampleHtml
                    expectedLoader = createLoaderScript [ScriptInfo "external.js" Nothing]
                in do
                    result `shouldSatisfy` T.isInfixOf (fromMaybe "" expectedLoader) -- Check loader content exists
                    result `shouldSatisfy` (not . T.isInfixOf "<script src=\"external.js\">") -- Check original removed
                    result `shouldSatisfy` T.isInfixOf ("</script></body></html>") -- Check loader is before </body>

            it "appends loader script if no </body>" $
                 let result = refactorSourcedScriptsForHashBasedCsp sampleHtmlNoBodyEnd
                     expectedLoader = createLoaderScript [ScriptInfo "external.js" Nothing]
                 in do
                     result `shouldSatisfy` T.isInfixOf (fromMaybe "" expectedLoader)
                     result `shouldSatisfy` (not . T.isInfixOf "<script src=\"external.js\">")
                     result `shouldSatisfy` T.isSuffixOf "</script>" -- Check loader is at the end

            it "does nothing if no sourced scripts" $
                 let htmlNoSourced = "<html><body><script>console.log(1)</script></body></html>"
                 in refactorSourcedScriptsForHashBasedCsp htmlNoSourced `shouldBe` htmlNoSourced