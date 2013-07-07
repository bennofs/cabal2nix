{ cabal, EdisonAPI, mtl, QuickCheck }:

cabal.mkDerivation (self: {
  pname = "EdisonCore";
  version = "1.2.2";
  sha256 = "13whgagqdh3q81h3b1xkpfiwwxhv8m77m60gh1avajaj2k0phc39";
  buildDepends = [ EdisonAPI mtl QuickCheck ];
  doCheck = false;
  meta = {
    homepage = "http://www.cs.princeton.edu/~rdockins/edison/home/";
    description = "A library of efficent, purely-functional data structures (Core Implementations)";
    license = "unknown";
    platforms = self.ghc.meta.platforms;
  };
})
