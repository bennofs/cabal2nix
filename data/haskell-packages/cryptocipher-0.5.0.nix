{ cabal, cereal, cipher-aes, cipher-rc4, cpu, crypto-api, cryptohash, entropy
, primitive, QuickCheck, test-framework, test-framework-quickcheck2, vector
}:

cabal.mkDerivation (self: {
  pname = "cryptocipher";
  version = "0.5.0";
  sha256 = "16gqsy23y3g9089ng94124g5pvc4d0vnh2r47ii789f8j96062nd";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    cereal cipher-aes cipher-rc4 cpu crypto-api primitive vector
  ];
  testDepends = [
    crypto-api cryptohash entropy QuickCheck test-framework
    test-framework-quickcheck2 vector
  ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/vincenthz/hs-cryptocipher";
    description = "Symmetrical block and stream ciphers";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
