{ cabal, cereal, certificate, cprng-aes, crypto-pubkey, crypto-random-api
, cryptohash, mtl, network, QuickCheck, test-framework
, test-framework-quickcheck2, time
}:

cabal.mkDerivation (self: {
  pname = "tls";
  version = "1.1.2";
  sha256 = "1vg1mnz6cxxgs48pbpjp4hwyvsysxyzvjfy4p1vd23lwc32cdjqg";
  buildDepends = [
    cereal certificate crypto-pubkey crypto-random-api cryptohash mtl network
  ];
  testDepends = [
    cereal certificate cprng-aes crypto-pubkey crypto-random-api mtl QuickCheck
    test-framework test-framework-quickcheck2 time
  ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/vincenthz/hs-tls";
    description = "TLS/SSL protocol native implementation (Server and Client)";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
