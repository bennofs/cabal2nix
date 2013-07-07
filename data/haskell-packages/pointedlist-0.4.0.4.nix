{ cabal, binary, derive, fclabels }:

cabal.mkDerivation (self: {
  pname = "pointedlist";
  version = "0.4.0.4";
  sha256 = "1nyvx4r6vbphl71crkbphp5jl7wanlbrxqai45l7ayk2aw2fvg39";
  buildDepends = [ binary derive fclabels ];
  doCheck = false;
  meta = {
    description = "A zipper-like comonad which works as a list, tracking a position";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
