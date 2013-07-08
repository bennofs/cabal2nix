{ cabal, cairo, glib, gtk, gtk2hs-buildtools, pango, webkit }:

cabal.mkDerivation (self: {
  pname = "webkit";
  version = "0.12.3";
  sha256 = "12r4vmdzhlcblyq3f6xr0mknjlqfj915lci56x4icjvxiyn8abc1";
  buildDepends = [ cairo glib gtk pango ];
  buildTools = [ gtk2hs-buildtools ];
  pkgconfigDepends = [ webkit ];
  doCheck = false;
  meta = {
    homepage = "http://projects.haskell.org/gtk2hs/";
    description = "Binding to the Webkit library";
    license = self.stdenv.lib.licenses.lgpl21;
    platforms = self.ghc.meta.platforms;
  };
})
