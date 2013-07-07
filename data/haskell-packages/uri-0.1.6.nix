{ cabal, parsec, safe, utf8-string }:

cabal.mkDerivation (self: {
  pname = "uri";
  version = "0.1.6";
  sha256 = "1dil29q9w9n352ksxddix21n0q3spd72pzjl49n1w2ifshsnq4gz";
  buildDepends = [ parsec safe utf8-string ];
  doCheck = false;
  meta = {
    homepage = "http://gitorious.org/uri";
    description = "Library for working with URIs";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
