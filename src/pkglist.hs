-- src/pkglist.hs
--
-- Build this program with cabal, and then run
--
--     dist/build/pkglist/pkglist data/packages.txt data/haskell-packages
--
-- to generate the corresponding Nix configuration for Hydra.

module Main ( main ) where

import Prelude hiding ( lookup )
import Cabal2Nix.Generate ( cabal2nix )
import Cabal2Nix.Hackage ( hashPackage )
import Cabal2Nix.Normalize ( normalizeList )
import Distribution.NixOS.Derivation.Cabal
import Distribution.Hackage.DB ( readHackage, PackageName(..), Package(..), (!) )
import Distribution.Compat.ReadP ( ReadP, char, munch, munch1, sepBy, option )
import Distribution.Text
import Text.PrettyPrint hiding ( char )
import Data.List
import System.FilePath
import System.Environment
import Control.Monad

type Comment = String

type DirectoryPath = FilePath

data Pkg = Pkg PackageName Version Comment
  deriving (Show, Eq, Ord)

spaces :: ReadP r String
spaces = munch1 (`elem` " \t")

instance Text Pkg where
  disp (Pkg pn pv cm) = hsep [disp pn, disp pv, text cm]
  parse = do pn <- parse
             _ <- spaces
             pv <- parse
             cm <- option "" (spaces >> munch (/='\n'))
             return (Pkg pn pv cm)

newtype PkgList = PkgList [Pkg]
  deriving (Show, Eq)

instance Text PkgList where
  disp (PkgList pl) = foldr (($+$) . disp) empty pl
  parse = fmap PkgList (sepBy parse (char '\n'))

readPkgList :: FilePath -> IO PkgList
readPkgList path = do r <- readFile path
                      case simpleParse r of
                        Just pl -> return pl
                        Nothing -> fail ("cannot parse package list " ++ show path)

writeNixFile :: DirectoryPath -> Derivation -> IO ()
writeNixFile dir pkg = do hash <- hashPackage pkgid
                          writeFile path (display pkg { sha256 = hash, doCheck = False })
  where pkgid = packageId pkg
        path = dir </> display pkgid ++ ".nix"

defaultNix :: [Derivation] -> String
defaultNix pkgs = unlines $
  [ "{ nixpkgs ? import <nixpkgs> {}, cabal ? nixpkgs.haskellPackages.cabal }:"
  , ""
  , "let"
  , ""
  , "  base-compat = null;"
  , "  binary = null;"
  , "  Cabal = null;"
  , "  checkers = null;"
  , "  concurrent-extra = null;"
  , "  deepseq = null;"
  , "  diagrams-contrib = null;"
  , "  dice = null;"
  , "  double-conversion = null;"
  , "  encoding = null;"
  , "  filepath = null;"
  , "  Glob = null;"
  , "  hexpat = null;"
  , "  hspec-meta = null;"
  , "  hstatsd = null;"
  , "  httpd-shed = null;"
  , "  libdpkg = null;"
  , "  markdown-unlit = null;"
  , "  misfortune = null;"
  , "  nanospec = null;"
  , "  process-leksah = null;"
  , "  random-fu = null;"
  , "  random-source = null;"
  , "  stringbuilder = null;"
  , "  test-framework-doctest = null;"
  , "  time = null;"
  , "  transformers-compat = null;"
  , "  uniqueid = null;"
  , "  utf8String = null;"
  , ""
  , "in"
  , ""
  , "rec {"
  , ""
  ] ++
  [ "" ++ callPackage p | p <- pkgs ] ++
  [ "}" ]

callPackage :: Derivation -> String
callPackage pkg = unlines $
  [ "  " ++ pname pkg ++ " = import ./" ++ display (packageId pkg) ++ ".nix {" ] ++
  [ "    inherit " ++ unwords ("cabal":hsInputs) ++ ";" ] ++
  (if null osInputs then [] else [ "    inherit (nixpkgs) " ++ unwords osInputs ++ ";" ]) ++
  [ "  };" ]
  where
    hsInputs = normalizeList (buildDepends pkg ++ testDepends pkg)
    osInputs = normalizeList (buildTools pkg ++ extraLibs pkg ++ pkgConfDeps pkg) \\ hsInputs

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 2) (fail "Usage: pkglist packages.txt output-dir")
  let pkglistFile:outputDir:[] = args
  hackage <- readHackage
  PkgList pkgs <- readPkgList pkglistFile
  let cabalFiles = [ hackage ! pn ! pv | Pkg (PackageName pn) pv _ <- pkgs ]
      derivations = map cabal2nix cabalFiles
  mapM_ (writeNixFile outputDir) derivations
  writeFile (outputDir </> "default.nix") (defaultNix derivations)
