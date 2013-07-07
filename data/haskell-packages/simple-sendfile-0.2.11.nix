{ cabal, conduit, hspec, HUnit, network, network-conduit }:

cabal.mkDerivation (self: {
  pname = "simple-sendfile";
  version = "0.2.11";
  sha256 = "1q9m9lxv9jfkn6a1lf07jcl4li3n5996df1qrfkfjq2n0bvn4qfj";
  buildDepends = [ network ];
  testDepends = [ conduit hspec HUnit network network-conduit ];
  doCheck = false;
  meta = {
    description = "Cross platform library for the sendfile system call";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
