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
import Distribution.Nixpkgs.Meta ( Meta( Meta ))
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
  MkDerivation
  { _pkgid = package
  , _revision = xrev
  , _src = DerivationSource
    { derivKind = "url"
    , derivUrl = mempty
    , derivRevision = mempty
    , derivHash = mempty
    }
  , _isLibrary = isJust library
  , _isExecutable = not (null executables)
  , _extraFunctionArgs = mempty
  , _libraryDepends = maybe mempty (convertBuildInfo . libBuildInfo) library
  , _executableDepends = mconcat (map (convertBuildInfo . buildInfo) executables)
  , _testDepends = mconcat (map (convertBuildInfo . testBuildInfo) testSuites)
  , _configureFlags = mempty
  , _cabalFlags = configureCabalFlags package
  , _runHaddock = True
  , _jailbreak = False
  , _doCheck = True
  , _testTarget = mempty
  , _hyperlinkSource = True
  , _enableSplitObjs = True
  , _phaseOverrides = mempty
  , _editedCabalFile = if xrev > 0 then fromJust (lookup "x-cabal-file-hash" customFieldsPD) else ""
  , Nix._metaSection = Meta
    { Nix._homepage = homepage
    , Nix._description = normalizeSynopsis synopsis
    , Nix._license = fromCabalLicense license
    , Nix._platforms = mempty
    , Nix._hydraPlatforms = mempty
    , Nix._maintainers = mempty
    , Nix._broken = False
    }
  }

convertBuildInfo :: Cabal.BuildInfo -> Nix.BuildInfo
convertBuildInfo Cabal.BuildInfo {..} = Nix.BuildInfo
  { _haskell = Set.fromList targetBuildDepends
  , _system = Set.unions [ Set.fromList buildTools, Set.fromList (map (\n -> Dependency (PackageName n) anyVersion) extraLibs) ]
  , _pkgconfig = Set.fromList pkgconfigDepends
  }
