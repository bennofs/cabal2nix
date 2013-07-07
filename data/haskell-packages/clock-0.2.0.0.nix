{ cabal }:

cabal.mkDerivation (self: {
  pname = "clock";
  version = "0.2.0.0";
  sha256 = "04lnvycws2hbf2gq9fvbm5ws96jd5hir1l2s72gvdpb8fw82qajg";
  doCheck = false;
  meta = {
    homepage = "http://corsis.sourceforge.net/index.php/Haskell/Clock";
    description = "High-resolution clock and timer functions: realtime, monotonic, cputime, etc";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
