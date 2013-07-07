{ cabal, attoparsec, base64-bytestring, bzlib, cereal, cereal-conduit, conduit
, crypto-pubkey, cryptohash, exception-transformers, HUnit, ixset, lens, mtl
, openpgp-asciiarmor, resourcet, split, test-framework, test-framework-hunit
, time, zlib
}:

cabal.mkDerivation (self: {
  pname = "hOpenPGP";
  version = "0.7";
  sha256 = "027p3rmqc2xr2qainr0qpj89ggdii1ifk91nlkhasnb6qyb0j0fy";
  buildDepends = [
    attoparsec base64-bytestring bzlib cereal cereal-conduit conduit
    crypto-pubkey cryptohash ixset lens mtl openpgp-asciiarmor split time zlib
  ];
  testDepends = [
    bzlib cereal cereal-conduit conduit crypto-pubkey cryptohash
    exception-transformers HUnit ixset lens resourcet split test-framework
    test-framework-hunit time zlib
  ];
  doCheck = false;
  meta = {
    homepage = "http://floss.scru.org/hOpenPGP/";
    description = "native Haskell implementation of OpenPGP (RFC4880)";
    license = "unknown";
    platforms = self.ghc.meta.platforms;
  };
})
