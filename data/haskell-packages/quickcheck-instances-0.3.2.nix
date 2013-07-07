{ cabal, QuickCheck, text, time }:

cabal.mkDerivation (self: {
  pname = "quickcheck-instances";
  version = "0.3.2";
  sha256 = "1is6lqis4gnn0m1yf83kxafnr78bhp3051h8cg0di7s9c2aw0yxy";
  buildDepends = [ QuickCheck text time ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/aslatter/qc-instances";
    description = "Common quickcheck instances";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
