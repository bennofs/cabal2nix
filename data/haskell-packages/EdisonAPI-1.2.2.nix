{ cabal, mtl }:

cabal.mkDerivation (self: {
  pname = "EdisonAPI";
  version = "1.2.2";
  sha256 = "1p0nlxkvnx57js0rl9589ysyv132rpr4pii5is51hcjbcr2dddn7";
  buildDepends = [ mtl ];
  doCheck = false;
  meta = {
    homepage = "http://www.cs.princeton.edu/~rdockins/edison/home/";
    description = "A library of efficent, purely-functional data structures (API)";
    license = "unknown";
    platforms = self.ghc.meta.platforms;
  };
})
