{ cabal, mtl }:

cabal.mkDerivation (self: {
  pname = "boomerang";
  version = "1.3.1";
  sha256 = "1516ysddlny085sg3065fi2jnbnvhdvp8qxl9fimv5xz40zbpgr4";
  buildDepends = [ mtl ];
  doCheck = false;
  meta = {
    description = "Library for invertible parsing and printing";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
