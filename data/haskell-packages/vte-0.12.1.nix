{ cabal, glib, gtk, gtk2hs-buildtools, pango, vte }:

cabal.mkDerivation (self: {
  pname = "vte";
  version = "0.12.1";
  sha256 = "1z9grhs161kx6jfrhxxwii722aj2h13fqfvbvl0ybdpm7imghnkl";
  buildDepends = [ glib gtk pango ];
  buildTools = [ gtk2hs-buildtools ];
  pkgconfigDepends = [ vte ];
  doCheck = false;
  meta = {
    homepage = "http://projects.haskell.org/gtk2hs/";
    description = "Binding to the VTE library";
    license = self.stdenv.lib.licenses.lgpl21;
    platforms = self.ghc.meta.platforms;
  };
})
