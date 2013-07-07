{ cabal, mersenne-random-pure64, MonadRandom }:

cabal.mkDerivation (self: {
  pname = "SimpleEA";
  version = "0.2.1";
  sha256 = "0qimm9y138jjnzy5i6s5b7899rkxahk7p2kcwfry0gdq2pcb68vr";
  buildDepends = [ mersenne-random-pure64 MonadRandom ];
  doCheck = false;
  meta = {
    homepage = "http://www.github.com/ehamberg/simpleea/";
    description = "Simple evolutionary algorithm framework";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
