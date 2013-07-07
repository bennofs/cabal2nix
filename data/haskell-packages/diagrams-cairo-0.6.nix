{ cabal, cairo, cmdargs, colour, diagrams-core, diagrams-lib, filepath, mtl
, split, time
}:

cabal.mkDerivation (self: {
  pname = "diagrams-cairo";
  version = "0.6";
  sha256 = "0fxqwkv2cpgpkr80q828rm91ybn7j0dwj1p5ysc3648w28jvhkil";
  buildDepends = [
    cairo cmdargs colour diagrams-core diagrams-lib filepath mtl split time
  ];
  doCheck = false;
  meta = {
    homepage = "http://projects.haskell.org/diagrams";
    description = "Cairo backend for diagrams drawing EDSL";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
