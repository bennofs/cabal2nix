{ cabal, mtl, text }:

cabal.mkDerivation (self: {
  pname = "hsp";
  version = "0.8.0";
  sha256 = "05r1bkw2xp79f81wfnl3pakcfg4cxpjf0rgb2vji7lgv5i8mlc9q";
  buildDepends = [ mtl text ];
  doCheck = false;
  meta = {
    homepage = "http://patch-tag.com/r/nibro/hsp";
    description = "Haskell Server Pages is a library for writing dynamic server-side web pages";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
