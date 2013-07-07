{ cabal, attoparsec, base64-bytestring, cereal, HUnit, test-framework
, test-framework-hunit
}:

cabal.mkDerivation (self: {
  pname = "openpgp-asciiarmor";
  version = "0.1";
  sha256 = "1xrv0n7n1n8izvxvqm8wmj6mkn5l6wcq18bxs9zd1q5riynmmm2w";
  buildDepends = [ attoparsec base64-bytestring cereal ];
  testDepends = [
    attoparsec base64-bytestring cereal HUnit test-framework
    test-framework-hunit
  ];
  doCheck = false;
  meta = {
    homepage = "http://floss.scru.org/openpgp-asciiarmor";
    description = "OpenPGP (RFC4880) ASCII Armor codec";
    license = "unknown";
    platforms = self.ghc.meta.platforms;
  };
})
