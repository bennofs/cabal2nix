{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards, DeriveGeneric, StandaloneDeriving#-}
{-# OPTIONS_GHC -fno-warn-orphans #-}           -- for FlagName below

module Distribution.Nixpkgs.Haskell where

import Control.DeepSeq.Generics
import Control.Lens
-- import Data.Char
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Function
import Data.List
import Data.Monoid ( Monoid(..) )
import Distribution.Version
import Distribution.Nixpkgs.Meta
import Distribution.Nixpkgs.Fetch
import Distribution.Nixpkgs.Util.PrettyPrinting
import Distribution.Package
import Distribution.PackageDescription ( FlagAssignment, FlagName(..) )
import GHC.Generics ( Generic )

data BuildInfo = BuildInfo
  { _haskell :: Set Dependency
  , _pkgconfig :: Set Dependency
  , _system :: Set Dependency
  }
  deriving (Show, Eq, Generic)

makeLenses ''BuildInfo

instance Monoid BuildInfo where
  mempty = BuildInfo mempty mempty mempty
  BuildInfo x1 y1 z1 `mappend` BuildInfo x2 y2 z2 = BuildInfo (x1 `mappend` x2) (y1 `mappend` y2) (z1 `mappend` z2)

-- | A represtation of Nix expressions for building Haskell packages.
-- The data type correspond closely to the definition of
-- 'PackageDescription' from Cabal.
--
-- Note that the "Text" instance definition provides pretty-printing,
-- but no parsing as of now!
data Derivation = MkDerivation
  { _pkgid               :: PackageIdentifier
  , _revision            :: Int
  , _src                 :: DerivationSource
  , _isLibrary           :: Bool
  , _isExecutable        :: Bool
  , _extraFunctionArgs   :: Set String
  , _libraryDepends      :: BuildInfo
  , _executableDepends   :: BuildInfo
  , _testDepends         :: BuildInfo
  , _configureFlags      :: Set String
  , _cabalFlags          :: FlagAssignment
  , _runHaddock          :: Bool
  , _jailbreak           :: Bool
  , _doCheck             :: Bool
  , _testTarget          :: String
  , _hyperlinkSource     :: Bool
  , _enableSplitObjs     :: Bool
  , _phaseOverrides      :: String
  , _editedCabalFile     :: String
  , _metaSection         :: Meta
  }
  deriving (Show, Eq, Generic)

makeLenses ''Derivation

instance Package Derivation where
  packageId = _pkgid

instance NFData BuildInfo where rnf = genericRnf

instance NFData Derivation where rnf = genericRnf

instance NFData FlagName where rnf = genericRnf

instance NFData VersionRange where rnf = genericRnf

instance NFData Dependency where rnf = genericRnf

instance Pretty Derivation where
  pPrint MkDerivation {..} = funargs (map text ("mkDerivation" : toAscList inputs)) $$ vcat
    [ text "mkDerivation" <+> lbrace
    , nest 2 $ vcat
      [ attr "pname"   $ doubleQuotes $ disp (packageName _pkgid)
      , attr "version" $ doubleQuotes $ disp (packageVersion _pkgid)
      , onlyIf (_revision > 0) $ attr "revision" $ doubleQuotes (int _revision)
      , sourceAttr _src
      , onlyIf (not (null _editedCabalFile)) $ attr "editedCabalFile" $ string _editedCabalFile
      , listattr "configureFlags" empty (map (show . show) renderedFlags)
      , boolattr "isLibrary" (not _isLibrary || _isExecutable) _isLibrary
      , boolattr "isExecutable" (not _isLibrary || _isExecutable) _isExecutable
      , onlyIf (_libraryDepends /= mempty) $ attr "libraryDepends" (pPrint _libraryDepends)
      , onlyIf (_executableDepends /= mempty) $ attr "executableDepends" (pPrint _executableDepends)
      , onlyIf (_testDepends /= mempty) $ attr "testDepends" (pPrint _testDepends)
      , boolattr "enableSplitObjs"  (not _enableSplitObjs) _enableSplitObjs
      , boolattr "doHaddock" (not _runHaddock) _runHaddock
      , boolattr "jailbreak" _jailbreak _jailbreak
      , boolattr "doCheck" (not _doCheck) _doCheck
      , onlyIf (not (null _testTarget)) $ attr "testTarget" $ string _testTarget
      , boolattr "hyperlinkSource" (not _hyperlinkSource) _hyperlinkSource
      , onlyIf (not (null _phaseOverrides)) $ vcat ((map text . lines) _phaseOverrides)
      , pPrint _metaSection
      ]
    , rbrace
    ]
    where
      inputs :: Set String
      inputs = Set.unions [ _extraFunctionArgs
                          , Set.map unDep (_haskell _libraryDepends)
                          , Set.map unDep (_haskell _executableDepends)
                          , Set.map unDep (_haskell _testDepends)
                          , Set.map unDep (_system _libraryDepends)
                          , Set.map unDep (_system _executableDepends)
                          , Set.map unDep (_system _testDepends)
                          , Set.map unDep (_pkgconfig _libraryDepends)
                          , Set.map unDep (_pkgconfig _executableDepends)
                          , Set.map unDep (_pkgconfig _testDepends)
                          , Set.fromList ["fetch" ++ derivKind _src | derivKind _src /= "" && not isHackagePackage]
                          ]

      renderedFlags = [ text "-f" <> (if enable then empty else char '-') <> text f | (FlagName f, enable) <- _cabalFlags ]
                      ++ map text (toAscList _configureFlags)
      isHackagePackage = "mirror://hackage/" `isPrefixOf` derivUrl _src
      sourceAttr (DerivationSource{..})
        | isHackagePackage = attr "sha256" $ string derivHash
        | derivKind /= "" = vcat
           [ text "src" <+> equals <+> text ("fetch" ++ derivKind) <+> lbrace
           , nest 2 $ vcat
             [ attr "url" $ string derivUrl
             , attr "sha256" $ string derivHash
             , if derivRevision /= "" then attr "rev" (string derivRevision) else empty
             ]
           , rbrace <> semi
           ]
        | otherwise = attr "src" $ text derivUrl

instance Pretty BuildInfo where
  pPrint BuildInfo {..} = fsep [lbrace, nest 2 attrs, rbrace]
    where
      attrs = fsep
              [ setattr "haskell" (Set.map unDep _haskell)
              , setattr "system"  (Set.map unDep _system)
              , setattr "pkgconfig" (Set.map unDep _pkgconfig)
              ]

unDep :: Dependency -> String
unDep (Dependency (PackageName x) _) = x

instance Ord Dependency where
  compare = compare `on` unDep
