{ cabal, glib, gtk, gtk2hs-buildtools, gtkC, libc, libglade, pkgconfig }:

cabal.mkDerivation (self: {
  pname = "glade";
  version = "0.12.1";
  sha256 = "114gdjz6bzfzqm71j17yb5mq96wcvjdv7ig3k4x4d9mdp97w8990";
  buildDepends = [ glib gtk ];
  buildTools = [ gtk2hs-buildtools ];
  extraLibraries = [ libc pkgconfig ];
  pkgconfigDepends = [ gtkC libglade ];
  doCheck = false;
  meta = {
    homepage = "http://projects.haskell.org/gtk2hs/";
    description = "Binding to the glade library";
    license = self.stdenv.lib.licenses.lgpl21;
    platforms = self.ghc.meta.platforms;
  };
})
