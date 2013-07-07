{ cabal, SDL }:

cabal.mkDerivation (self: {
  pname = "SDL-gfx";
  version = "0.6.0";
  sha256 = "14d8fz576rwi6x0dxgc29cdmwn48afja3v5qx3x8q5y61fv8w9v1";
  buildDepends = [ SDL ];
  doCheck = false;
  meta = {
    description = "Binding to libSDL_gfx";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
