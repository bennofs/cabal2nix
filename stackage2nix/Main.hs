{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Distribution.Package (PackageName, PackageIdentifier(..), Dependency(..), packageName)
import Distribution.PackageDescription (GenericPackageDescription(..))
import Distribution.Nixpkgs.Haskell.PackageSourceSpec
import Distribution.Nixpkgs.Haskell.FromCabal
import Distribution.Nixpkgs.Fetch
import Distribution.Nixpkgs.Haskell.Derivation
import Distribution.Nixpkgs.Haskell.BuildInfo
import Distribution.Nixpkgs.PackageMap (readNixpkgPackageMap, resolve, PackageMap)
import Distribution.Version (withinRange)
import Language.Haskell.Extension (Language(Haskell98, Haskell2010))
import Language.Nix
import Data.Version
import HackageGit
import Control.Lens
import Data.Maybe
import Data.Ord
import Data.Foldable (maximumBy)
import System.IO (withFile, IOMode(..), Handle, hPutStrLn)

import qualified Data.Aeson as Aeson
import qualified Data.Yaml as Yaml
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Graph as Graph

data Options = Options
  { optBuildPlanFile :: FilePath
  , optAllCabalHashes :: FilePath
  , optNixpkgsRepository :: FilePath
  , optNixpkgsMap :: Maybe FilePath
  , optOutPackages :: FilePath
  , optOutConfig :: FilePath
  }

options :: Parser Options
options = Options
  <$> strArgument (metavar "PLAN" <> help "stackage build plan (YAML)")
  <*> strArgument (metavar "CABALFILES" <> help "path to checkout of all-cabal-hashes")
  <*> strOption (long "nixpkgs" <> help "path to Nixpkgs repository" <> value "<nixpkgs>" <> showDefaultWith id <> metavar "PATH")
  <*> optional (strOption (long "package-map" <> help "path to a serialized nixpkgs package map" <> metavar "PATH"))
  <*> strOption (long "out-packages" <> help "name of the output file for the package set" <> value "packages.nix" <> metavar "PATH")
  <*> strOption (long "out-config" <> help "name of the output file for the package set configuration" <> value "configuration-packages.nix" <> metavar "PATH")

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

data Node = Node
  { nodeName :: !String
  , nodeTestDepends :: !(Set.Set String)
  , nodeOtherDepends :: !(Set.Set String)
  }

nodeDepends :: Node -> Set.Set String
nodeDepends = nodeTestDepends <> nodeOtherDepends

generatePackage :: Handle -> PackageSetConfig -> PackageName -> PackagePlan -> IO Node
generatePackage h conf name plan = do
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
    haskellDependencies s
      = Set.map (view (localName . ident))
      . Set.filter isFromHackage
      $ view (s . (haskell <> tool)) drv
      
  hPutStrLn h . render . nest 2 $
    hang (hsep [doubleQuotes (text (display name)), equals, text "callPackage"]) 2 $
      parens (pPrint drv) <+> (braces overrides <> semi)

  return $! Node
    { nodeName = display . packageName . view pkgid $ drv
    , nodeTestDepends = haskellDependencies testDepends
    , nodeOtherDepends = haskellDependencies (executableDepends <> libraryDepends)
    }

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
buildPlanContainsDependency packageVersions (Dependency depName versionRange) =
  maybe False (`withinRange` versionRange) $ Map.lookup depName packageVersions

newtype SerializablePackageMap = SerializablePackageMap { getPackageMap :: PackageMap }

instance Aeson.ToJSON SerializablePackageMap where
  toJSON (SerializablePackageMap m) = Aeson.object . map makeEntry $ Map.toList m
   where
    makeEntry (i, s) = (Text.pack (view ident i), Aeson.toJSON . map makePath $ Set.toList s)
    makePath = view (path . mapping ident)

instance Aeson.FromJSON SerializablePackageMap where
  parseJSON = Aeson.withObject "package map object" $
    fmap (SerializablePackageMap . Map.fromList) . traverse makeEntry . HashMap.toList
   where
    makeEntry (i, s) = Aeson.parseJSON s <&> \s' -> (ident # Text.unpack i, Set.fromList (map makePath s'))
    makePath = review (path . mapping ident)

findCycles :: [Node] -> [[Node]]
findCycles nodes = mapMaybe cyclic $
  Graph.stronglyConnComp [(node, nodeName node, Set.toList $ nodeDepends node) | node <- nodes]
 where
  cyclic (Graph.AcyclicSCC _) = Nothing
  cyclic (Graph.CyclicSCC c) = Just c

breakCycle :: [Node] -> [String]
breakCycle [] = []
breakCycle c = nodeName breaker : concatMap breakCycle (findCycles remaining)
 where
  breaker = maximumBy (comparing rate) c
  remaining = map updateNode c
  updateNode n
    | nodeName n == nodeName breaker = n { nodeTestDepends = mempty }
    | otherwise = n
  names = Set.fromList $ map nodeName c
  rate node = - Set.size (nodeOtherDepends node `Set.intersection` names)


main :: IO ()
main = do
  Options{..} <- execParser pinfo

  nixpkgs <- fromMaybe (readNixpkgPackageMap optNixpkgsRepository Nothing) $ do
    mapFile <- optNixpkgsMap
    return $ do
      contents <- ByteString.readFile mapFile
      case Aeson.eitherDecodeStrict contents of
        Left err -> fail $ "Failed to decode package map " ++ show mapFile ++ ": " ++ err
        Right v -> return $ getPackageMap v

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

  nodes <- withFile optOutPackages WriteMode $ \h -> do
    hPutStrLn h ("# Generated by stackage2nix from " ++ optBuildPlanFile)
    hPutStrLn h ""
    hPutStrLn h ("{ pkgs, stdenv, callPackage }:")
    hPutStrLn h ""
    hPutStrLn h "self: {"
    hPutStrLn h ""
    nodes <- mapM (uncurry (generatePackage h conf)) (Map.toList (bpPackages buildPlan))
    hPutStrLn h "}"
    return nodes

  withFile optOutConfig WriteMode $ \h -> do
    hPutStrLn h ("# Generated by stackage2nix from " ++ optBuildPlanFile)
    hPutStrLn h "{ pkgs }:"
    hPutStrLn h ""
    hPutStrLn h "with pkgs.haskell.lib; self: super: {"
    hPutStrLn h ""

    hPutStrLn h "  # core packages"
    forM_ (Map.toList (siCorePackages systemInfo)) $ \(pkg, _version) -> do
      when (pkg /= "ghc") . hPutStrLn h . render . nest 2 $
        hsep [doubleQuotes (text (display pkg)), equals, text "null"] <> semi
    hPutStrLn h ""

    forM_ (findCycles nodes) $ \c -> do
      hPutStrLn h $ "  # break cycle: " ++ unwords (map nodeName c)
      forM_ (breakCycle c) $ \breaker -> do
        hPutStrLn h . render . nest 2 $
          hsep [doubleQuotes (text breaker), equals, text "dontCheck", text "super." <> text breaker] <> semi
    hPutStrLn h "}"
