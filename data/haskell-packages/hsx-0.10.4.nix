{ cabal, haskell-src-exts, mtl, utf8-string }:

cabal.mkDerivation (self: {
  pname = "hsx";
  version = "0.10.4";
  sha256 = "1d1l0bw9im5j51hna00d7477svd61xhqk6j145d4gf86arcq60id";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [ haskell-src-exts mtl utf8-string ];
  doCheck = false;
  meta = {
    homepage = "http://patch-tag.com/r/nibro/hsx";
    description = "HSX (Haskell Source with XML) allows literal XML syntax in Haskell source code";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
