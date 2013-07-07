{ cabal, alex, binary, deepseq, filepath, geniplate, happy, hashable
, hashtables, haskeline, haskell-src-exts, mtl, parallel, QuickCheck, text
, time, unordered-containers, xhtml, zlib
}:

cabal.mkDerivation (self: {
  pname = "Agda";
  version = "2.3.2.1";
  sha256 = "1dlf0cs913ma8wjvra8x6p0lwi1pk7ynbdq4lxgbdfgqkbnh43kr";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    binary deepseq filepath geniplate hashable hashtables haskeline
    haskell-src-exts mtl parallel QuickCheck text time unordered-containers
    xhtml zlib
  ];
  buildTools = [ alex happy ];
  doCheck = false;
  meta = {
    homepage = "http://wiki.portal.chalmers.se/agda/";
    description = "A dependently typed functional programming language and proof assistant";
    license = "unknown";
    platforms = self.ghc.meta.platforms;
  };
})
