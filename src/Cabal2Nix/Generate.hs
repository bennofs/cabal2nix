{-# LANGUAGE RecordWildCards #-}

module Cabal2Nix.Generate ( cabal2nix, cabal2nix' ) where

import Cabal2Nix.Flags
-- import Cabal2Nix.License
import Cabal2Nix.Normalize
-- import Cabal2Nix.PostProcess
import Control.Lens
-- import Data.Set.Lens
-- import Control.Applicative
import Data.Maybe
import qualified Data.Set as Set
import Data.Version
import Data.Monoid
import Distribution.Nixpkgs.Util.PrettyPrinting
import Distribution.Compiler
import Distribution.Nixpkgs.Haskell
import qualified Distribution.Nixpkgs.Haskell as Nix
import Cabal2Nix.License
import Distribution.Nixpkgs.Fetch
import qualified Distribution.Nixpkgs.Meta as Nix
import Distribution.Package
import Distribution.Version
import Distribution.PackageDescription
import qualified Distribution.PackageDescription as Cabal
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.System
import Distribution.Verbosity

cabal2nix :: FlagAssignment -> GenericPackageDescription -> Derivation
cabal2nix flags' cabal = drv & cabalFlags .~ flags
  where drv = cabal2nix' descr
        flags = normalizeCabalFlags (flags' ++ configureCabalFlags (package (packageDescription cabal)))
        Right (descr, _) = finalizePackageDescription
                            flags
                            (const True)
                            (Platform X86_64 Linux)                 -- shouldn't be hardcoded
                            (unknownCompilerInfo (CompilerId GHC (Version [7,10,1] [])) NoAbiTag)
                            []
                            cabal

cabal2nix' :: PackageDescription -> Derivation
cabal2nix' PackageDescription {..} =
  let
    xrev = maybe 0 read (lookup "x-revision" customFieldsPD)
  in
  undefined
  & pkgid .~ package
  & revision .~ xrev
  & src .~ DerivationSource
             { derivKind = "url"
             , derivUrl = mempty
             , derivRevision = mempty
             , derivHash = mempty
             }
  & isLibrary .~ isJust library
  & isExecutable .~ not (null executables)
  & extraFunctionArgs .~ mempty
  & libraryDepends .~ maybe mempty (convertBuildInfo . libBuildInfo) library
  & executableDepends .~ mconcat (map (convertBuildInfo . buildInfo) executables)
  & testDepends .~ mconcat (map (convertBuildInfo . testBuildInfo) testSuites)
  & configureFlags .~ mempty
  & cabalFlags .~ configureCabalFlags package
  & runHaddock .~ True
  & jailbreak .~ False
  & doCheck .~ True
  & testTarget .~ mempty
  & hyperlinkSource .~ True
  & enableSplitObjs .~ True
  & phaseOverrides .~ mempty
  & editedCabalFile .~ (if xrev > 0 then fromJust (lookup "x-cabal-file-hash" customFieldsPD) else "")
  & metaSection.(Nix.homepage) .~ homepage
  & metaSection.(Nix.description) .~ normalizeSynopsis synopsis
  & metaSection.(Nix.license) .~ fromCabalLicense license
  & metaSection.(Nix.platforms) .~ mempty
  & metaSection.(Nix.hydraPlatforms) .~ mempty
  & metaSection.(Nix.maintainers) .~ mempty
  & metaSection.(Nix.broken) .~ False

convertBuildInfo :: Cabal.BuildInfo -> Nix.BuildInfo
convertBuildInfo Cabal.BuildInfo {..} = Nix.BuildInfo
  { _haskell = Set.fromList targetBuildDepends
  , _system = Set.unions [ Set.fromList buildTools, Set.fromList (map (\n -> Dependency (PackageName n) anyVersion) extraLibs) ]
  , _pkgconfig = Set.fromList pkgconfigDepends
  }
