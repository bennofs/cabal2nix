{ cabal, glib, gtk, gtk2hs-buildtools, gtksourceview, libc, mtl, pkgconfig }:

cabal.mkDerivation (self: {
  pname = "gtksourceview2";
  version = "0.12.3.1";
  sha256 = "1c91ja753dzr2c7sv13wn47sjbjg45jv8xx9ybx1q3188xrldqng";
  buildDepends = [ glib gtk mtl ];
  buildTools = [ gtk2hs-buildtools ];
  extraLibraries = [ libc pkgconfig ];
  pkgconfigDepends = [ gtksourceview ];
  doCheck = false;
  meta = {
    homepage = "http://projects.haskell.org/gtk2hs/";
    description = "Binding to the GtkSourceView library";
    license = self.stdenv.lib.licenses.lgpl21;
    platforms = self.ghc.meta.platforms;
  };
})
