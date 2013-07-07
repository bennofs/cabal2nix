{ cabal, blaze-builder, Cabal, happy, HUnit, mtl, QuickCheck, test-framework
, test-framework-hunit, utf8-light, utf8-string
}:

cabal.mkDerivation (self: {
  pname = "language-javascript";
  version = "0.5.6";
  sha256 = "1a1nki2z7x0rna5jg6g0gqnipvd115k4xgagg6prrvj284ml44wd";
  buildDepends = [ blaze-builder mtl utf8-light utf8-string ];
  testDepends = [
    blaze-builder Cabal HUnit mtl QuickCheck test-framework
    test-framework-hunit utf8-light utf8-string
  ];
  buildTools = [ happy ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/alanz/language-javascript";
    description = "Parser for JavaScript";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
