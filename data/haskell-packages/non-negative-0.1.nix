{ cabal, QuickCheck, utility-ht }:

cabal.mkDerivation (self: {
  pname = "non-negative";
  version = "0.1";
  sha256 = "071mdj7p3ina71nalaixss23p84p9529h1r324ms14c1a7svdsqa";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [ QuickCheck utility-ht ];
  doCheck = false;
  meta = {
    homepage = "http://code.haskell.org/~thielema/non-negative/";
    description = "Non-negative numbers";
    license = "GPL";
    platforms = self.ghc.meta.platforms;
  };
})
