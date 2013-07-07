{ cabal, mtl, tokyocabinet }:

cabal.mkDerivation (self: {
  pname = "tokyocabinet-haskell";
  version = "0.0.5";
  sha256 = "1v6s39q8a6cnc0ggpspz9i0xw6aih4ixn9bhn4hwf9kwgcspardg";
  buildDepends = [ mtl ];
  extraLibraries = [ tokyocabinet ];
  doCheck = false;
  meta = {
    homepage = "http://tom-lpsd.github.com/tokyocabinet-haskell/";
    description = "Haskell binding of Tokyo Cabinet";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
