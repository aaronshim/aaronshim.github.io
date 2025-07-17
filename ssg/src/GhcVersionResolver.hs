{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GhcVersionResolver
  ( GhcVersionInfo(..)
  , resolveGhcVersion
  , ghcVersionContext
  , FlakeLock(..)
  , FlakeNodes(..)
  , FlakeNode(..)
  , FlakeNodeLocked(..)
  , parseLatestVerMap
  , extractMajorVersion
  , findVersionInMap
  , resolveGhcVersionFromHaskellNix
  ) where

import Control.Exception (try, SomeException)
import Data.Aeson (FromJSON(..), (.:), withObject, eitherDecode)
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Hakyll (Context, field, Compiler, unsafeCompiler)
import Network.HTTP.Req ((/:), GET(..), NoReqBody(..), defaultHttpConfig, req, responseBody, runReq, https, bsResponse)
import System.Directory (doesFileExist)

-- | Information about the GHC version used in the build
data GhcVersionInfo = GhcVersionInfo
  { ghcVersion :: T.Text
  , ghcNixpkgsUrl :: T.Text
  } deriving (Show, Generic)

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

-- | Resolve GHC version information from flake.lock and flake.nix
-- This function is designed to be resilient and never crash the build.
-- It returns Nothing if any step fails, allowing the site to build without GHC version info.
resolveGhcVersion :: IO (Maybe GhcVersionInfo)
resolveGhcVersion = do
  result <- try $ do
    -- Check if flake.lock exists
    flakeLockExists <- doesFileExist "flake.lock"
    if not flakeLockExists
      then return Nothing
      else do
        -- Parse flake.lock with error handling
        flakeLockResult <- try $ do
          flakeLockContent <- L.readFile "flake.lock"
          case eitherDecode flakeLockContent of
            Left _ -> return Nothing
            Right flakeLock -> return $ Just flakeLock
        case flakeLockResult of
          Left (_ :: SomeException) -> return Nothing
          Right Nothing -> return Nothing
          Right (Just flakeLock) -> do
            -- Get compiler-nix-name from flake.nix with error handling
            compilerNixName <- try getCompilerNixName
            case compilerNixName of
              Left (_ :: SomeException) -> return Nothing
              Right Nothing -> return Nothing
              Right (Just name) -> do
                -- Get commit hashes safely
                let haskellNixRev = rev $ locked $ haskellNix $ nodes flakeLock
                let nixpkgsRev = rev $ locked $ nixpkgsUnstable $ nodes flakeLock
                
                -- Resolve GHC version from haskell.nix bootstrap file with error handling
                ghcVer <- resolveGhcVersionFromHaskellNix haskellNixRev name
                case ghcVer of
                  Nothing -> return Nothing
                  Just version -> do
                    let nixpkgsUrl = "https://github.com/NixOS/nixpkgs/blob/" 
                                   <> nixpkgsRev 
                                   <> "/pkgs/development/compilers/ghc/" 
                                   <> version 
                                   <> ".nix"
                    return $ Just $ GhcVersionInfo version nixpkgsUrl
  case result of
    Left (_ :: SomeException) -> return Nothing
    Right info -> return info

-- | Extract compiler-nix-name from flake.nix
getCompilerNixName :: IO (Maybe T.Text)
getCompilerNixName = do
  flakeNixExists <- doesFileExist "flake.nix"
  if not flakeNixExists
    then return Nothing
    else do
      content <- readFile "flake.nix"
      -- Search for compiler-nix-name = "ghcXXX"
      let lines' = lines content
      let compilerLine = filter (T.isInfixOf "compiler-nix-name" . T.pack) lines'
      case compilerLine of
        [] -> return Nothing
        (line:_) -> do
          -- Extract ghcXXX from the line
          let lineTxt = T.pack line
          case T.splitOn "\"" lineTxt of
            (_:_:name:_) -> return $ Just name
            _ -> return Nothing

-- | Resolve GHC version by reading haskell.nix bootstrap file at specific commit
resolveGhcVersionFromHaskellNix :: T.Text -> T.Text -> IO (Maybe T.Text)
resolveGhcVersionFromHaskellNix haskellNixRev compilerName = do
  result <- try $ do
    -- Fetch the bootstrap.nix file from GitHub using req
    let url = https "raw.githubusercontent.com" /: "input-output-hk" /: "haskell.nix" /: haskellNixRev /: "overlays" /: "bootstrap.nix"
    
    response <- runReq defaultHttpConfig $ do
      req GET url NoReqBody bsResponse mempty
    
    let content = TE.decodeUtf8 $ responseBody response
    
    -- Parse the latestVerMap from bootstrap.nix
    parseLatestVerMap content compilerName
    
  case result of
    Left (_ :: SomeException) -> return Nothing
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
        [a, b, c, d] -> Just $ T.pack [a, '.', b, c]  -- ghc9101 -> "9.10" 
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