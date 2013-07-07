{ cabal, HUnit, mtl, mtlparse }:

cabal.mkDerivation (self: {
  pname = "regexpr";
  version = "0.5.4";
  sha256 = "0136wp6hrnmj6pbdhp3a131dzz8bp1pbd92bpagpv1r6gwj16y5z";
  buildDepends = [ HUnit mtl mtlparse ];
  doCheck = false;
  meta = {
    homepage = "http://homepage3.nifty.com/salamander/second/projects/regexpr/";
    description = "regular expression like Perl/Ruby in Haskell";
    license = "LGPL";
    platforms = self.ghc.meta.platforms;
  };
})
