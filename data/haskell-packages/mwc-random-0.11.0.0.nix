{ cabal, HUnit, primitive, QuickCheck, statistics, test-framework
, test-framework-hunit, test-framework-quickcheck2, time, vector
}:

cabal.mkDerivation (self: {
  pname = "mwc-random";
  version = "0.11.0.0";
  sha256 = "1yqi472m3snx71fvd4mig6my74rkpf3sbsdcmx2y2l00cyk79ghh";
  buildDepends = [ primitive time vector ];
  testDepends = [
    HUnit QuickCheck statistics test-framework test-framework-hunit
    test-framework-quickcheck2
  ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/bos/mwc-random";
    description = "Fast, high quality pseudo random number generation";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
