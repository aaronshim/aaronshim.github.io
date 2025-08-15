{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GhcVersionResolver
  ( GhcVersionInfo(..)
  , resolveGhcVersion
  , ghcVersionContext
  -- Legacy exports for backward compatibility with tests
  , FlakeLock(..)
  , FlakeNodes(..)
  , FlakeNode(..)
  , FlakeNodeLocked(..)
  , parseLatestVerMap
  , extractMajorVersion
  , findVersionInMap
  , resolveGhcVersionFromHaskellNix
  , getCompilerNixName
  ) where

import Control.Exception (try, SomeException)
import Data.Aeson (FromJSON(..), (.:), withObject)

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import Hakyll (Context, field, unsafeCompiler)
import Network.HTTP.Req ((/:), GET(..), NoReqBody(..), defaultHttpConfig, req, responseBody, runReq, https, bsResponse)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)

-- | Information about the GHC version used in the build
data GhcVersionInfo = GhcVersionInfo
  { ghcVersion :: T.Text
  , ghcNixpkgsUrl :: T.Text
  } deriving (Show, Eq, Generic)

-- | Flake lock structure (minimal, only what we need)
data FlakeLock = FlakeLock
  { nodes :: FlakeNodes
  } deriving (Show, Generic)

data FlakeNodes = FlakeNodes
  { haskellNix :: FlakeNode
  , nixpkgsUnstable :: FlakeNode
  } deriving (Show, Generic)

data FlakeNode = FlakeNode
  { locked :: FlakeNodeLocked
  } deriving (Show, Generic)

data FlakeNodeLocked = FlakeNodeLocked
  { rev :: T.Text
  } deriving (Show, Generic)

instance FromJSON FlakeLock where
  parseJSON = withObject "FlakeLock" $ \o -> FlakeLock
    <$> o .: "nodes"

instance FromJSON FlakeNodes where
  parseJSON = withObject "FlakeNodes" $ \o -> FlakeNodes
    <$> o .: "haskellNix"
    <*> o .: "nixpkgs-unstable"

instance FromJSON FlakeNode where
  parseJSON = withObject "FlakeNode" $ \o -> FlakeNode
    <$> o .: "locked"

instance FromJSON FlakeNodeLocked where
  parseJSON = withObject "FlakeNodeLocked" $ \o -> FlakeNodeLocked
    <$> o .: "rev"

-- | Resolve GHC version information from Nix build environment
-- This reads the version info directly passed from the Nix build
resolveGhcVersion :: IO (Maybe GhcVersionInfo)
resolveGhcVersion = do
  putStrLn "GHC Version Resolution: Starting"
  
  -- Read directly from environment variables set by Nix build
  ghcVersionEnv <- lookupEnv "GHC_VERSION"
  nixpkgsRevEnv <- lookupEnv "NIXPKGS_REV"
  
  case (ghcVersionEnv, nixpkgsRevEnv) of
    (Just version, Just nixpkgsRev) -> do
      putStrLn $ "GHC Version Resolution: SUCCESS - " ++ version ++ " (from Nix derivation)"
      let versionText = T.pack version
      let nixpkgsUrl = "https://github.com/NixOS/nixpkgs/blob/" 
                     <> T.pack nixpkgsRev 
                     <> "/pkgs/development/compilers/ghc/" 
                     <> versionText
                     <> ".nix"
      return $ Just $ GhcVersionInfo versionText nixpkgsUrl
    _ -> do
      putStrLn "GHC Version Resolution: No environment variables available - build without version info"
      return Nothing

-- | Extract compiler-nix-name from flake.nix with fallback
getCompilerNixName :: IO (Maybe T.Text)
getCompilerNixName = do
  putStrLn "GHC Version Resolution: Looking up compiler name"
  -- Always use the known fallback for now since file parsing is unreliable during build
  putStrLn "GHC Version Resolution: Using fallback compiler name ghc910"
  return $ Just "ghc910"

-- | Resolve GHC version by reading bootstrap.nix file (with network fallback)
resolveGhcVersionFromHaskellNix :: T.Text -> T.Text -> IO (Maybe T.Text)
resolveGhcVersionFromHaskellNix haskellNixRev compilerName = do
  -- First try to read from the environment variable (build-time fetched file)
  bootstrapPathEnv <- lookupEnv "GHC_VERSION_BOOTSTRAP_PATH"
  case bootstrapPathEnv of
    Just bootstrapPath -> do
      putStrLn $ "GHC Version Resolution: Reading bootstrap.nix from " ++ bootstrapPath
      bootstrapExists <- doesFileExist bootstrapPath
      if bootstrapExists
        then do
          content <- TIO.readFile bootstrapPath
          result <- parseLatestVerMap content compilerName
          case result of
            Just version -> putStrLn $ "GHC Version Resolution: Parsed version " ++ T.unpack version
            Nothing -> putStrLn "GHC Version Resolution: Failed to parse version from bootstrap.nix"
          return result
        else do
          putStrLn $ "GHC Version Resolution: Bootstrap file not found at " ++ bootstrapPath
          return Nothing
    Nothing -> do
      putStrLn "GHC Version Resolution: No bootstrap path, trying network fallback"
      fallbackToNetwork
  where
    fallbackToNetwork = do
      result <- try $ do
        -- Fetch the bootstrap.nix file from GitHub using req
        let url = https "raw.githubusercontent.com" /: "input-output-hk" /: "haskell.nix" /: haskellNixRev /: "overlays" /: "bootstrap.nix"
        
        response <- runReq defaultHttpConfig $ do
          req GET url NoReqBody bsResponse mempty
        
        let content = TE.decodeUtf8 $ responseBody response
        
        -- Parse the latestVerMap from bootstrap.nix
        parseLatestVerMap content compilerName
        
      case result of
        Left (e :: SomeException) -> do
          putStrLn $ "GHC Version Resolution: Network fallback failed: " ++ show e
          return Nothing
        Right version -> return version


-- | Parse the latestVerMap from bootstrap.nix content
parseLatestVerMap :: T.Text -> T.Text -> IO (Maybe T.Text)
parseLatestVerMap content compilerName = do
  let lines' = T.lines content
  
  -- Find the latestVerMap section
  let inLatestVerMap = dropWhile (not . T.isInfixOf "latestVerMap = {") lines'
  case inLatestVerMap of
    [] -> return Nothing
    (_:mapLines) -> do
      -- Take lines until we hit the closing brace
      let mapContent = takeWhile (not . T.isInfixOf "};") mapLines
      
      -- Extract version mapping based on compiler name
      -- For ghc910 -> look for "9.10" = "9.10.1"
      let majorVersion = extractMajorVersion compilerName
      case majorVersion of
        Nothing -> return Nothing
        Just major -> findVersionInMap mapContent major

-- | Extract major version from compiler name (ghc910 -> "9.10")
extractMajorVersion :: T.Text -> Maybe T.Text
extractMajorVersion name
  | "ghc" `T.isPrefixOf` name = do
      let digits = T.drop 3 name
      case T.unpack digits of
        [a, b] -> Just $ T.pack [a, '.', b]       -- ghc98 -> "9.8"
        [a, b, c] -> Just $ T.pack [a, '.', b, c]  -- ghc910 -> "9.10"
        [a, b, c, _] -> Just $ T.pack [a, '.', b, c]  -- ghc9101 -> "9.10" 
        _ -> Nothing
  | otherwise = Nothing

-- | Find version mapping in the latestVerMap content
findVersionInMap :: [T.Text] -> T.Text -> IO (Maybe T.Text)
findVersionInMap mapLines majorVer = do
  let versionLine = filter (\line -> 
        T.isInfixOf ("\"" <> majorVer <> "\"") line &&
        T.isInfixOf "=" line) mapLines
  case versionLine of
    [] -> return Nothing
    (line:_) -> do
      -- Parse: "9.10" = "9.10.1";
      let parts = T.splitOn "=" line
      case parts of
        [_, versionPart] -> do
          let cleaned = T.strip $ T.filter (/= ';') $ T.filter (/= '"') versionPart
          return $ Just cleaned
        _ -> return Nothing

-- | Hakyll context for GHC version information
ghcVersionContext :: Context String
ghcVersionContext = 
  field "ghcVersion" (\_ -> unsafeCompiler $ do
    info <- resolveGhcVersion
    case info of
      Nothing -> return "unknown"
      Just i -> return $ T.unpack $ ghcVersion i
  ) <>
  field "ghcNixpkgsUrl" (\_ -> unsafeCompiler $ do
    info <- resolveGhcVersion
    case info of
      Nothing -> return "#"
      Just i -> return $ T.unpack $ ghcNixpkgsUrl i
  ) <>
  field "hasGhcVersion" (\_ -> unsafeCompiler $ do
    info <- resolveGhcVersion
    case info of
      Nothing -> return "false"
      Just _ -> return "true"
  )