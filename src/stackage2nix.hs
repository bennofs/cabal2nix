{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Cabal2Nix.HackageGit ( readHackage, Hackage )
import Cabal2Nix.Version
import Control.Lens
import Control.Monad.Par.IO
import Control.Monad.Trans
import Distribution.Compiler
import Distribution.Nixpkgs.Haskell
import Distribution.Nixpkgs.Haskell.Generate
import Distribution.Nixpkgs.PackageMap
import Distribution.Package
import Distribution.System
import Distribution.Version
import Stackage.Types
import System.IO
import System.Exit
import Options.Applicative

import qualified Data.Yaml.Aeson as Yaml
import qualified Data.Map.Lazy as Map

data Options = Options
  { hackageRepository :: FilePath
  , nixpkgsRepository :: FilePath
  , stackageSnapshot  :: FilePath
  }
  deriving (Show)

options :: Parser Options
options = Options
          <$> strOption (long "hackage" <> help "path to Hackage git repository" <> value "hackage" <> showDefault <> metavar "PATH")
          <*> strOption (long "nixpkgs" <> help "path to Nixpkgs repository" <> value "<nixpkgs>" <> showDefault <> metavar "PATH")
          <*> strArgument (metavar "SNAPSHOT" <> help "path to a stackage build plan (.yaml)")

pinfo :: ParserInfo Options
pinfo = info
        (   helper
        <*> infoOption ("stackage2nix " ++ version) (long "version" <> help "Show version number")
        <*> options
        )
        (  fullDesc
        <> header "stackage2nix converts a stackage build plan into a nix haskell package set"
        )

main :: IO ()
main = do
  Options {..} <- execParser pinfo

  (_, hackage) <- readHackage hackageRepository
  nixpkgs <- readNixpkgPackageMap nixpkgsRepository
  buildPlan <- parseSnapshot stackageSnapshot

  let config = generateConfiguration hackage buildPlan
  runParIO $ writePackageSet nixpkgs . resolvePackageSet $ config   

parseSnapshot :: FilePath -> IO BuildPlan
parseSnapshot file = Yaml.decodeFileEither file >>= \case
  Left err -> do
    hPutStrLn stderr $ "Snapshot parsing error: " ++ Yaml.prettyPrintParseException err
    exitFailure
  Right bp -> return bp

generateConfiguration :: Hackage -> BuildPlan -> PackageSetConfig
generateConfiguration hackage BuildPlan{..} = PackageSetConfig
  { platform = Platform (siArch bpSystemInfo) (siOS bpSystemInfo)
  , compilerInfo = unknownCompilerInfo (CompilerId GHC (siGhcVersion bpSystemInfo)) NoAbiTag    
  , corePackages = map (uncurry PackageIdentifier) $ Map.toList $ siCorePackages bpSystemInfo 
  , defaultPackages = Map.mapWithKey (configurePackage hackage) packages
  , extraPackages = Map.empty   
  }
  where 
    getPackageName (PackageName name) = name
    packages = Map.mapKeysMonotonic getPackageName bpPackages

testStateToStatus :: TestState -> TestStatus
testStateToStatus ExpectSuccess = EnableTests
testStateToStatus ExpectFailure = DisableTests
testStateToStatus Don'tBuild = DisableTests

configurePackage :: Hackage -> String -> PackagePlan -> (Version, ResolveM [Dependency])
configurePackage hackage name PackagePlan{..} = (,) ppVersion $ do
  pkg <- liftIO $ hackage Map.! name Map.! ppVersion
  let 
    test = testStateToStatus $ pcTests ppConstraints
    setHaddock = runHaddock .~ (pcHaddocks ppConstraints == ExpectSuccess)
    flags = Map.toList (pcFlagOverrides ppConstraints)
  mapSuccess setHaddock $ resolve test flags [] pkg
