{ cabal }:

cabal.mkDerivation (self: {
  pname = "HJavaScript";
  version = "0.4.7";
  sha256 = "0sb2wqbf6kml5d414xi6jk0gr31673djqxa5wg1mxl40vwn14pvh";
  doCheck = false;
  meta = {
    description = "HJavaScript is an abstract syntax for a typed subset of JavaScript";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
