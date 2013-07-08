{ cabal, glib, gtk, gtk2hs-buildtools, gtkglext, pango }:

cabal.mkDerivation (self: {
  pname = "gtkglext";
  version = "0.12.1";
  sha256 = "1vf8jczhnrmrylkzj190wgvn1yxzgkkmd16ghdsxv66q8vm91p5q";
  buildDepends = [ glib gtk pango ];
  buildTools = [ gtk2hs-buildtools ];
  pkgconfigDepends = [ gtkglext ];
  doCheck = false;
  meta = {
    homepage = "http://projects.haskell.org/gtk2hs/";
    description = "Binding to the GTK+ OpenGL Extension";
    license = self.stdenv.lib.licenses.lgpl21;
    platforms = self.ghc.meta.platforms;
  };
})
