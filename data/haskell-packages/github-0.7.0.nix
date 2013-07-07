{ cabal, aeson, attoparsec, case-insensitive, conduit, data-default, failure
, HTTP, http-conduit, http-types, network, text, time, unordered-containers
, vector
}:

cabal.mkDerivation (self: {
  pname = "github";
  version = "0.7.0";
  sha256 = "0r803hpyyd0nfhlk5jn4ripzi2cpj708zp9g961g7wvvvi66013p";
  buildDepends = [
    aeson attoparsec case-insensitive conduit data-default failure HTTP
    http-conduit http-types network text time unordered-containers vector
  ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/fpco/github";
    description = "Access to the Github API, v3";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
