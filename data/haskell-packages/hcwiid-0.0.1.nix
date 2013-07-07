{ cabal, bluetooth, cwiid }:

cabal.mkDerivation (self: {
  pname = "hcwiid";
  version = "0.0.1";
  sha256 = "0aar3j1ms96m60c9c8v6bbk2dvjn32ixmdkqcn8jxb8wpq42xna1";
  extraLibraries = [ bluetooth cwiid ];
  doCheck = false;
  meta = {
    homepage = "https://gitorious.org/hcwiid";
    description = "Library to interface with the wiimote";
    license = self.stdenv.lib.licenses.gpl2;
    platforms = self.ghc.meta.platforms;
  };
})
