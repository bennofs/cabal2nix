{ cabal, aeson, attoparsec, conduit, hspec, HUnit, resourcet, text
, transformers, unordered-containers, vector
}:

cabal.mkDerivation (self: {
  pname = "yaml";
  version = "0.8.2.3";
  sha256 = "1ds6969gbkxgkm2fha0ifmssjl7by9glgix165v0h8i7fx9wx3wa";
  buildDepends = [
    aeson attoparsec conduit resourcet text transformers unordered-containers
    vector
  ];
  testDepends = [ conduit hspec HUnit text transformers unordered-containers ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/snoyberg/yaml/";
    description = "Support for parsing and rendering YAML documents";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
