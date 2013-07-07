{ cabal, attoparsec, BoundedChan, bytestring-lexing, HUnit, mtl, network
, resource-pool, test-framework, test-framework-hunit, time, vector
}:

cabal.mkDerivation (self: {
  pname = "hedis";
  version = "0.6.2";
  sha256 = "0dlkmy509pyycdicq7lfqyvd87zkhr8pyjblq0q1wd55js2kr1cp";
  buildDepends = [
    attoparsec BoundedChan bytestring-lexing mtl network resource-pool time
    vector
  ];
  testDepends = [ HUnit mtl test-framework test-framework-hunit time ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/informatikr/hedis";
    description = "Client library for the Redis datastore: supports full command set, pipelining";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
