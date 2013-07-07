{ cabal, base64-bytestring, cereal, cipher-aes, cprng-aes, crypto-api, entropy
, hspec, HUnit, QuickCheck, skein, tagged, transformers
}:

cabal.mkDerivation (self: {
  pname = "clientsession";
  version = "0.9";
  sha256 = "0cyw34vzvv1j7w094cjcf97g8bki7l9x82s8csaf96y6d9qws308";
  buildDepends = [
    base64-bytestring cereal cipher-aes cprng-aes crypto-api entropy skein
    tagged
  ];
  testDepends = [ cereal hspec HUnit QuickCheck transformers ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/yesodweb/clientsession/tree/master";
    description = "Securely store session data in a client-side cookie";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
