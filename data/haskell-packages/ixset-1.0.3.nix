{ cabal, Cabal, HUnit, QuickCheck, random, safecopy, syb, syb-with-class }:

cabal.mkDerivation (self: {
  pname = "ixset";
  version = "1.0.3";
  sha256 = "0fik338javxb8l309bxi6n6yp94rvl8p3gz2pnf7alj4vxyzf9s4";
  buildDepends = [ safecopy syb syb-with-class ];
  testDepends = [ Cabal HUnit QuickCheck random ];
  doCheck = false;
  meta = {
    homepage = "http://happstack.com";
    description = "Efficient relational queries on Haskell sets";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
