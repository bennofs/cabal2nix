{ cabal, mtl, text }:

cabal.mkDerivation (self: {
  pname = "template";
  version = "0.2.0.8";
  sha256 = "0563bcjh1mcfim11wbwbqpy19bgx901cnrf51s58rm1hz3baladv";
  buildDepends = [ mtl text ];
  doCheck = false;
  meta = {
    description = "Simple string substitution";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
