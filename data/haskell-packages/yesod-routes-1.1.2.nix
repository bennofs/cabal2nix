{ cabal, hspec, HUnit, path-pieces, text, vector }:

cabal.mkDerivation (self: {
  pname = "yesod-routes";
  version = "1.1.2";
  sha256 = "17yv2z7k1s958xyn552wpk0cwpivsnkvia8477yhgbp4n2d1i5jv";
  buildDepends = [ path-pieces text vector ];
  testDepends = [ hspec HUnit path-pieces text ];
  doCheck = false;
  meta = {
    homepage = "http://www.yesodweb.com/";
    description = "Efficient routing for Yesod";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
