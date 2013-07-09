{ cabal, GConf, glib, gtk2hs-buildtools }:

cabal.mkDerivation (self: {
  pname = "gconf";
  version = "0.12.1.1";
  sha256 = "1nn4py7nvqjvlzwc1fk6gfvmjfywi7r59j63nb93pkj7d65fnxvn";
  buildDepends = [ glib ];
  buildTools = [ gtk2hs-buildtools ];
  pkgconfigDepends = [ GConf ];
  doCheck = false;
  meta = {
    homepage = "http://projects.haskell.org/gtk2hs/";
    description = "Binding to the GNOME configuration database system";
    license = self.stdenv.lib.licenses.lgpl21;
    platforms = self.ghc.meta.platforms;
  };
})
