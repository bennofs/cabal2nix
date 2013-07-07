{ cabal, OpenAL, StateVar }:

cabal.mkDerivation (self: {
  pname = "ALUT";
  version = "2.2.0.0";
  sha256 = "0i1s1sxap65han83wyil8rfb911rc704iajy0lgn87f5a0sglpwx";
  buildDepends = [ OpenAL StateVar ];
  doCheck = false;
  meta = {
    homepage = "http://connect.creativelabs.com/openal/";
    description = "A binding for the OpenAL Utility Toolkit";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
