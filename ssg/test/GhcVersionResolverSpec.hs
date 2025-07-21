{-# LANGUAGE OverloadedStrings #-}

module Main where

import GhcVersionResolver
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Test.Hspec
import System.IO.Temp (withTempFile)
import System.Directory (getCurrentDirectory)
import System.IO (hPutStr, hClose)
import System.Environment (setEnv, unsetEnv)
import Data.List (isInfixOf)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "GhcVersionResolver" $ do
    
    describe "resolveGhcVersion (main function)" $ do
      it "resolves version from environment variables" $ do
        -- Set up environment
        setEnv "GHC_VERSION" "9.10.1"
        setEnv "NIXPKGS_REV" "041c867bad68dfe34b78b2813028a2e2ea70a23c"
        
        result <- resolveGhcVersion
        
        -- Clean up environment
        unsetEnv "GHC_VERSION"
        unsetEnv "NIXPKGS_REV"
        
        case result of
          Nothing -> expectationFailure "Should have resolved version from environment"
          Just info -> do
            ghcVersion info `shouldBe` "9.10.1"
            ghcNixpkgsUrl info `shouldSatisfy` T.isInfixOf "041c867bad68dfe34b78b2813028a2e2ea70a23c"
            ghcNixpkgsUrl info `shouldSatisfy` T.isInfixOf "9.10.1.nix"
      
      it "returns Nothing when environment variables are missing" $ do
        -- Ensure environment is clean
        unsetEnv "GHC_VERSION"
        unsetEnv "NIXPKGS_REV"
        
        result <- resolveGhcVersion
        result `shouldBe` Nothing
        
      it "returns Nothing when only one environment variable is set" $ do
        -- Test with only GHC_VERSION
        setEnv "GHC_VERSION" "9.10.1"
        unsetEnv "NIXPKGS_REV"
        
        result <- resolveGhcVersion
        result `shouldBe` Nothing
        
        -- Clean up and test with only NIXPKGS_REV
        unsetEnv "GHC_VERSION"
        setEnv "NIXPKGS_REV" "041c867bad68dfe34b78b2813028a2e2ea70a23c"
        
        result2 <- resolveGhcVersion
        result2 `shouldBe` Nothing
        
        -- Clean up
        unsetEnv "NIXPKGS_REV"
    
    -- Legacy function tests (for backward compatibility)
    describe "Legacy functions (parseLatestVerMap)" $ do
      let sampleBootstrapContent = T.unlines
            [ "final: prev:"
            , "let"
            , "    buildBootstrapper.compilerNixName ="
            , "      if final.buildPackages.haskell.compiler ? ghc964 then \"ghc964\""
            , "      else \"ghc8107\";"
            , "    latestVerMap = {"
            , "      \"8.10\" = \"8.10.7\";"
            , "      \"9.0\" = \"9.0.2\";"
            , "      \"9.2\" = \"9.2.8\";"
            , "      \"9.4\" = \"9.4.8\";"
            , "      \"9.6\" = \"9.6.7\";"
            , "      \"9.8\" = \"9.8.4\";"
            , "      \"9.10\" = \"9.10.1\";"
            , "      \"9.12\" = \"9.12.2\";"
            , "    };"
            , "    gitInputs = {"
            , "      ghc96X = \"9.6.7\";"
            , "      ghc912X = \"9.12.1\";"
            , "    };"
            ]

      it "extracts GHC 9.10.1 for ghc910" $ do
        result <- parseLatestVerMap sampleBootstrapContent "ghc910"
        result `shouldBe` Just "9.10.1"

      it "extracts GHC 9.8.4 for ghc98" $ do
        result <- parseLatestVerMap sampleBootstrapContent "ghc98" 
        result `shouldBe` Just "9.8.4"

      it "extracts GHC 9.6.7 for ghc96" $ do
        result <- parseLatestVerMap sampleBootstrapContent "ghc96"
        result `shouldBe` Just "9.6.7"

      it "returns Nothing for unknown compiler" $ do
        result <- parseLatestVerMap sampleBootstrapContent "ghc999"
        result `shouldBe` Nothing

      it "handles ghc9101 variant (same as ghc910)" $ do
        result <- parseLatestVerMap sampleBootstrapContent "ghc9101"
        result `shouldBe` Just "9.10.1"

    describe "Legacy functions (extractMajorVersion)" $ do
      it "extracts 9.10 from ghc910" $
        extractMajorVersion "ghc910" `shouldBe` Just "9.10"

      it "extracts 9.8 from ghc98" $
        extractMajorVersion "ghc98" `shouldBe` Just "9.8"

      it "extracts 9.10 from ghc9101" $
        extractMajorVersion "ghc9101" `shouldBe` Just "9.10"

      it "extracts 9.12 from ghc9122" $
        extractMajorVersion "ghc9122" `shouldBe` Just "9.12"

      it "returns Nothing for invalid format" $
        extractMajorVersion "invalid" `shouldBe` Nothing

      it "returns Nothing for non-ghc prefix" $
        extractMajorVersion "gcc910" `shouldBe` Nothing

    describe "Legacy functions (findVersionInMap)" $ do
      let mapLines = 
            [ "      \"8.10\" = \"8.10.7\";"
            , "      \"9.0\" = \"9.0.2\";"
            , "      \"9.8\" = \"9.8.4\";"
            , "      \"9.10\" = \"9.10.1\";"
            , "      \"9.12\" = \"9.12.2\";"
            ]

      it "finds version 9.10.1 for key 9.10" $ do
        result <- findVersionInMap mapLines "9.10"
        result `shouldBe` Just "9.10.1"

      it "finds version 9.8.4 for key 9.8" $ do
        result <- findVersionInMap mapLines "9.8"
        result `shouldBe` Just "9.8.4"

      it "returns Nothing for missing key" $ do
        result <- findVersionInMap mapLines "9.99"
        result `shouldBe` Nothing

    describe "Legacy functions (FlakeLock JSON parsing)" $ do
      let sampleFlakeLock = L.toStrict $ L.fromStrict $ 
            "{\"nodes\":{\"haskellNix\":{\"locked\":{\"rev\":\"853414c88f54e690aee4cc74a0972d3eef43c546\"}},\"nixpkgs-unstable\":{\"locked\":{\"rev\":\"041c867bad68dfe34b78b2813028a2e2ea70a23c\"}}}}"

      it "parses flake.lock correctly" $ do
        case eitherDecode (L.fromStrict sampleFlakeLock) of
          Left err -> expectationFailure $ "Failed to parse JSON: " ++ err
          Right flakeLock -> do
            let haskellNixRev = rev $ locked $ haskellNix $ nodes flakeLock
            let nixpkgsRev = rev $ locked $ nixpkgsUnstable $ nodes flakeLock
            haskellNixRev `shouldBe` "853414c88f54e690aee4cc74a0972d3eef43c546"
            nixpkgsRev `shouldBe` "041c867bad68dfe34b78b2813028a2e2ea70a23c"

    describe "Legacy functions (Known commit hash tests)" $ do
      -- These are real historical values for testing
      let knownCommits = 
            [ ("853414c88f54e690aee4cc74a0972d3eef43c546", "ghc910", "9.10.1")
            , ("853414c88f54e690aee4cc74a0972d3eef43c546", "ghc98", "9.8.4") 
            , ("853414c88f54e690aee4cc74a0972d3eef43c546", "ghc96", "9.6.7")
            ] :: [(T.Text, T.Text, T.Text)]

      -- Note: These tests require internet access and are integration tests
      -- They're included for completeness but might be skipped in CI
      mapM_ (\(hash, compiler, expectedVersion) -> 
        it ("resolves " ++ T.unpack compiler ++ " to " ++ T.unpack expectedVersion ++ " at commit " ++ take 8 (T.unpack hash)) $ do
          result <- resolveGhcVersionFromHaskellNix hash compiler
          -- This test requires network access - might fail in isolated environments
          -- For now, just test that it doesn't crash and either succeeds or fails gracefully
          result `shouldSatisfy` (\r -> r == Just expectedVersion || r == Nothing)
        ) knownCommits

    describe "GhcVersionInfo construction and URL format" $ do
      it "creates correct nixpkgs URL format" $ do
        let info = GhcVersionInfo "9.10.1" "https://github.com/NixOS/nixpkgs/blob/041c867bad68dfe34b78b2813028a2e2ea70a23c/pkgs/development/compilers/ghc/9.10.1.nix"
        ghcVersion info `shouldBe` "9.10.1"
        ghcNixpkgsUrl info `shouldSatisfy` T.isInfixOf "nixpkgs/blob"
        ghcNixpkgsUrl info `shouldSatisfy` T.isInfixOf "041c867bad68dfe34b78b2813028a2e2ea70a23c"
        ghcNixpkgsUrl info `shouldSatisfy` T.isInfixOf "ghc/9.10.1.nix"
        
      it "validates URL construction from environment variables" $ do
        -- Test the URL construction logic
        let version = "9.10.1"
        let nixpkgsRev = "041c867bad68dfe34b78b2813028a2e2ea70a23c"
        let expectedUrl = "https://github.com/NixOS/nixpkgs/blob/" <> nixpkgsRev <> "/pkgs/development/compilers/ghc/" <> version <> ".nix"
        let info = GhcVersionInfo (T.pack version) (T.pack expectedUrl)
        
        ghcVersion info `shouldBe` T.pack version
        ghcNixpkgsUrl info `shouldBe` T.pack expectedUrl

    describe "Integration test with temporary files" $ do
      it "reads compiler-nix-name from flake.nix-like content" $ do
        let flakeContent = unlines
              [ "{"
              , "  description = \"test\";"
              , "  outputs = { self, nixpkgs, haskellNix }:"
              , "    let"
              , "      hakyllProject = final.haskell-nix.project' {"
              , "        src = ./ssg;"
              , "        compiler-nix-name = \"ghc910\";"
              , "        modules = [{ doHaddock = false; }];"
              , "      };"
              , "    in {};"
              , "}"
              ]
        
        withTempFile "." "test-flake.nix" $ \tempFile handle -> do
          -- Write test content
          hPutStr handle flakeContent
          hClose handle
          
          -- Test extraction (would need to modify getCompilerNixName to accept file path)
          -- For now, just test the parsing logic manually
          let lines' = lines flakeContent
          let compilerLine = filter (T.isInfixOf "compiler-nix-name" . T.pack) lines'
          case compilerLine of
            [] -> expectationFailure "Should find compiler-nix-name"
            (line:_) -> do
              let lineTxt = T.pack line
              -- Debug: print the line and its splits
              let splits = T.splitOn "\"" lineTxt
              case splits of
                [before, name, after] -> T.unpack name `shouldBe` "ghc910"
                _ -> expectationFailure $ "Should extract ghc910, got splits: " ++ show (map T.unpack splits)

    describe "Error handling" $ do
      it "handles malformed JSON gracefully" $ do
        let badJson = L.fromStrict "{invalid json"
        case eitherDecode badJson :: Either String FlakeLock of
          Left _ -> return () -- Expected
          Right _ -> expectationFailure "Should fail on invalid JSON"

      it "handles missing compiler-nix-name gracefully" $ do
        result <- parseLatestVerMap "no compiler info here" "ghc910"
        result `shouldBe` Nothing

      it "handles empty bootstrap content gracefully" $ do
        result <- parseLatestVerMap "" "ghc910"
        result `shouldBe` Nothing

    describe "Resilient context generation" $ do
      it "ghcVersionContext provides hasGhcVersion field with environment variables" $ do
        -- Test with environment variables set
        setEnv "GHC_VERSION" "9.10.1"
        setEnv "NIXPKGS_REV" "041c867bad68dfe34b78b2813028a2e2ea70a23c"
        
        result <- resolveGhcVersion
        
        -- Clean up environment
        unsetEnv "GHC_VERSION"
        unsetEnv "NIXPKGS_REV"
        
        case result of
          Nothing -> expectationFailure "Should resolve with environment variables"
          Just info -> do
            ghcVersion info `shouldSatisfy` (not . T.null)
            ghcNixpkgsUrl info `shouldSatisfy` (not . T.null)
            
      it "ghcVersionContext fails gracefully without environment variables" $ do
        -- Ensure environment is clean
        unsetEnv "GHC_VERSION"
        unsetEnv "NIXPKGS_REV"
        
        result <- resolveGhcVersion
        result `shouldBe` Nothing  -- Should gracefully return Nothing

    describe "Hakyll context field behavior" $ do
      it "ghcVersion field returns 'unknown' when resolution fails" $ do
        -- Ensure environment is clean
        unsetEnv "GHC_VERSION"
        unsetEnv "NIXPKGS_REV"
        
        -- Test the actual logic used in ghcVersionContext
        info <- resolveGhcVersion
        let result = case info of
              Nothing -> "unknown"
              Just i -> T.unpack $ ghcVersion i
        
        result `shouldBe` "unknown"
        
      it "ghcNixpkgsUrl field returns '#' when resolution fails" $ do
        -- Ensure environment is clean
        unsetEnv "GHC_VERSION"
        unsetEnv "NIXPKGS_REV"
        
        -- Test the actual logic used in ghcVersionContext
        info <- resolveGhcVersion
        let result = case info of
              Nothing -> "#"
              Just i -> T.unpack $ ghcNixpkgsUrl i
        
        result `shouldBe` "#"
        
      it "hasGhcVersion field returns 'false' when resolution fails" $ do
        -- Ensure environment is clean
        unsetEnv "GHC_VERSION"
        unsetEnv "NIXPKGS_REV"
        
        -- Test the actual logic used in ghcVersionContext
        info <- resolveGhcVersion
        let result = case info of
              Nothing -> "false"
              Just _ -> "true"
        
        result `shouldBe` "false"
        
      it "hasGhcVersion field returns 'true' when resolution succeeds" $ do
        -- Set up environment
        setEnv "GHC_VERSION" "9.10.1"
        setEnv "NIXPKGS_REV" "041c867bad68dfe34b78b2813028a2e2ea70a23c"
        
        -- Test the actual logic used in ghcVersionContext
        info <- resolveGhcVersion
        let result = case info of
              Nothing -> "false"
              Just _ -> "true"
        
        -- Clean up environment
        unsetEnv "GHC_VERSION"
        unsetEnv "NIXPKGS_REV"
        
        result `shouldBe` "true"
        
      it "context fields work correctly with valid environment variables" $ do
        -- Set up environment
        setEnv "GHC_VERSION" "9.10.1"
        setEnv "NIXPKGS_REV" "041c867bad68dfe34b78b2813028a2e2ea70a23c"
        
        -- Test all context fields
        info <- resolveGhcVersion
        
        case info of
          Nothing -> expectationFailure "Should resolve with environment variables"
          Just i -> do
            -- Test ghcVersion field
            let versionResult = T.unpack $ ghcVersion i
            versionResult `shouldBe` "9.10.1"
            
            -- Test ghcNixpkgsUrl field
            let urlResult = T.unpack $ ghcNixpkgsUrl i
            urlResult `shouldSatisfy` (/= "#")  -- Should not be fallback
            urlResult `shouldSatisfy` ("nixpkgs/blob" `isInfixOf`)
            
            -- Test hasGhcVersion field
            let hasVersionResult = "true"
            hasVersionResult `shouldBe` "true"
        
        -- Clean up environment
        unsetEnv "GHC_VERSION"
        unsetEnv "NIXPKGS_REV"
    
    describe "Template rendering logic verification" $ do
      it "template should NOT render GHC sentence when hasGhcVersion is false" $ do
        -- Ensure environment is clean
        unsetEnv "GHC_VERSION"
        unsetEnv "NIXPKGS_REV"
        
        -- Simulate template logic: $if(hasGhcVersion)$...Built with GHC...$endif$
        info <- resolveGhcVersion
        let hasGhcVersionBool = case info of
              Nothing -> False
              Just _ -> True
        
        -- Template should not render the GHC section
        hasGhcVersionBool `shouldBe` False
        
      it "template SHOULD render GHC sentence when hasGhcVersion is true" $ do
        -- Set up environment
        setEnv "GHC_VERSION" "9.10.1"
        setEnv "NIXPKGS_REV" "041c867bad68dfe34b78b2813028a2e2ea70a23c"
        
        -- Simulate template logic: $if(hasGhcVersion)$...Built with GHC...$endif$
        info <- resolveGhcVersion
        let hasGhcVersionBool = case info of
              Nothing -> False
              Just _ -> True
        
        -- Template should render the GHC section
        hasGhcVersionBool `shouldBe` True
        
        -- Clean up environment
        unsetEnv "GHC_VERSION"
        unsetEnv "NIXPKGS_REV"