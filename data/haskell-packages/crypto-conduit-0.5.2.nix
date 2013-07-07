{ cabal, cereal, conduit, crypto-api, cryptocipher, cryptohash-cryptoapi, hspec
, skein, transformers
}:

cabal.mkDerivation (self: {
  pname = "crypto-conduit";
  version = "0.5.2";
  sha256 = "0ncqwr2a9nxl6q7qys9gb5db62lx622g5db1xhpfni045x324kbz";
  buildDepends = [ cereal conduit crypto-api transformers ];
  testDepends = [
    cereal conduit crypto-api cryptocipher cryptohash-cryptoapi hspec skein
    transformers
  ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/meteficha/crypto-conduit";
    description = "Conduit interface for cryptographic operations (from crypto-api)";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
