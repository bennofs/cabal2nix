{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import Options.Applicative
import Stackage.BuildPlan
import Stackage.Types (SystemInfo(..), PackageConstraints(..), DepInfo(..), SimpleDesc(..), TestState(..))
import Paths_cabal2nix as Main
import Distribution.Text (display, disp)
import Control.Monad
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Distribution.Compiler (CompilerInfo(..), AbiTag(NoAbiTag), CompilerId(..), CompilerFlavor(GHC))
import Distribution.System (Platform(..))
import Distribution.Package (PackageName, PackageIdentifier(..), Dependency(..))
import Distribution.PackageDescription (GenericPackageDescription(..))
import Distribution.Nixpkgs.Haskell.PackageSourceSpec
import Distribution.Nixpkgs.Haskell.FromCabal
import Distribution.Nixpkgs.Fetch
import Distribution.Nixpkgs.Haskell.Derivation
import Distribution.Nixpkgs.PackageMap (readNixpkgPackageMap, resolve)
import Distribution.Version (withinRange)
import Language.Haskell.Extension (Language(Haskell98, Haskell2010))
import Language.Nix
import Data.Version
import HackageGit
import Control.Lens
import Data.Maybe

import qualified Data.Yaml as Yaml
import qualified Data.Map as Map
import qualified Data.Set as Set

data Options = Options
  { optBuildPlanFile :: FilePath
  , optAllCabalHashes :: FilePath
  , optNixpkgsRepository :: FilePath
  }

options :: Parser Options
options = Options
  <$> strArgument (metavar "PLAN" <> help "stackage build plan (YAML)")
  <*> strArgument (metavar "CABALFILES" <> help "path to checkout of all-cabal-hashes")
  <*> strOption (long "nixpkgs" <> help "path to Nixpkgs repository" <> value "<nixpkgs>" <> showDefaultWith id <> metavar "PATH")

pinfo :: ParserInfo Options
pinfo = info
        (   helper
        <*> infoOption ("stackage2nix " ++ display Main.version) (long "version" <> help "Show version number")
        <*> options
        )
        (  fullDesc
        <> header "stackage2nix converts stackage snapshots into a nix package set"
        )

loadBuildPlan :: FilePath -> IO BuildPlan
loadBuildPlan = Yaml.decodeFileEither >=> \case
  Left err -> fail $ "Failed to parse stackage build plan: " ++ show err
  Right bp -> return bp

data PackageSetConfig = PackageSetConfig
  { haskellResolver :: HaskellResolver
  , nixpkgsResolver :: NixpkgsResolver
  , packageLoader   :: PackageIdentifier -> IO Package
  , targetPlatform  :: Platform
  , targetCompiler  :: CompilerInfo
  }

generatePackage :: PackageSetConfig -> PackageName -> PackagePlan -> IO ()
generatePackage conf name plan = do
  pkg <- packageLoader conf $ PackageIdentifier name (ppVersion plan)
  
  let
    constraints = ppConstraints plan
    testsEnabled = pcTests constraints == ExpectSuccess
    haddocksEnabled
       = pcHaddocks constraints == ExpectSuccess
      && not (Set.null (sdModules (ppDesc plan)))
    configureTests
      | pcTests constraints == Don'tBuild = removeTests
      | otherwise = id
    genericDrv = fromGenericPackageDescription
      (haskellResolver conf)
      (nixpkgsResolver conf)
      (targetPlatform conf)
      (targetCompiler conf)
      (Map.toList (pcFlagOverrides constraints))
      (planDependencies plan)
      (configureTests (pkgCabal pkg))
    drv = genericDrv
      & src .~ pkgSource pkg
      & doCheck .~ testsEnabled
      & runHaddock .~ haddocksEnabled
    overrides = fsep
      [ disp bind <> semi
      | bind <- Set.toList $ view (dependencies . each <> extraFunctionArgs) drv
      , not $ isFromHackage bind
      ]
      
  putStrLn . render . nest 2 $
    hang (hsep [doubleQuotes (text (display name)), equals, text "callPackage"]) 2 $
      parens (pPrint drv) <+> (braces overrides <> semi)

removeTests :: GenericPackageDescription -> GenericPackageDescription
removeTests gd = gd { condTestSuites = [] }

isFromHackage :: Binding -> Bool
isFromHackage b = case view (reference . path) b of
                    ["self",_] -> True
                    _ -> False

planDependencies :: PackagePlan -> [Dependency]
planDependencies = map makeDependency . Map.toList . sdPackages . ppDesc
 where
  makeDependency (name, depInfo) = Dependency name (diRange depInfo)

loadPackage :: FilePath -> PackageIdentifier -> IO Package
loadPackage allCabalHashesPath pkgId = do
  (pkgDesc, _) <- readPackage allCabalHashesPath pkgId
  meta <- readPackageMeta allCabalHashesPath pkgId
  let
    tarballSHA256 = fromMaybe
      (error (display pkgId ++ ": meta data has no SHA256 hash for the tarball"))
      (view (hashes . at "SHA256") meta)
    source = DerivationSource "url" ("mirror://hackage/" ++ display pkgId ++ ".tar.gz") "" tarballSHA256
  return $ Package source pkgDesc

ghcCompilerInfo :: Version -> CompilerInfo
ghcCompilerInfo v = CompilerInfo
  { compilerInfoId = CompilerId GHC v
  , compilerInfoAbiTag = NoAbiTag
  , compilerInfoCompat = Just []
  , compilerInfoLanguages = Just [Haskell98, Haskell2010]
  , compilerInfoExtensions = Nothing
  }

buildPlanContainsDependency :: Map.Map PackageName Version -> Dependency -> Bool
buildPlanContainsDependency packageVersions (Dependency packageName versionRange) =
  maybe False (`withinRange` versionRange) $ Map.lookup packageName packageVersions

main :: IO ()
main = do
  Options{..} <- execParser pinfo

  nixpkgs <- readNixpkgPackageMap optNixpkgsRepository Nothing

  buildPlan <- loadBuildPlan optBuildPlanFile

  let
    systemInfo = bpSystemInfo buildPlan
    packageVersions = fmap ppVersion (bpPackages buildPlan) `Map.union` siCorePackages systemInfo
    conf = PackageSetConfig
      { packageLoader = loadPackage optAllCabalHashes
      , targetPlatform = Platform (siArch systemInfo) (siOS systemInfo)
      , targetCompiler = ghcCompilerInfo (siGhcVersion systemInfo)
      , nixpkgsResolver = resolve (Map.map (Set.map (over path ("pkgs":))) nixpkgs)
      , haskellResolver = buildPlanContainsDependency packageVersions
      }

  putStrLn ("# Generated by stackage2nix from " ++ optBuildPlanFile)
  putStrLn ""
  putStrLn ("{ pkgs, stdenv, callPackage }:")
  putStrLn ""
  putStrLn "self: {"
  putStrLn ""
  mapM_ (uncurry $ generatePackage conf) (Map.toList (bpPackages buildPlan))
  putStrLn "}"

  return ()
