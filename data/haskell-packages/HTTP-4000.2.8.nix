{ cabal, case-insensitive, conduit, deepseq, http-types, httpd-shed, HUnit, mtl
, network, parsec, pureMD5, split, test-framework, test-framework-hunit, wai
, warp
}:

cabal.mkDerivation (self: {
  pname = "HTTP";
  version = "4000.2.8";
  sha256 = "0p0cwzjw2102bsyfaga6m8b53s6qnhd6byg2j2qla653f6kjlsh8";
  buildDepends = [ mtl network parsec ];
  testDepends = [
    case-insensitive conduit deepseq http-types httpd-shed HUnit mtl network
    pureMD5 split test-framework test-framework-hunit wai warp
  ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/haskell/HTTP";
    description = "A library for client-side HTTP";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
