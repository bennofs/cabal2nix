{ cabal, HJavaScript, hsx, mtl }:

cabal.mkDerivation (self: {
  pname = "HJScript";
  version = "0.6.1";
  sha256 = "1qnh1xswj2l7m1b1b7dm2117p5y60xpfdsfafmcgyh1bjym7b5n3";
  buildDepends = [ HJavaScript hsx mtl ];
  doCheck = false;
  meta = {
    homepage = "http://patch-tag.com/r/nibro/hjscript";
    description = "HJScript is a Haskell EDSL for writing JavaScript programs";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
