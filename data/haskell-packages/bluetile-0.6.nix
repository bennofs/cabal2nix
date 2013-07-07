{ cabal, ConfigFile, filepath, glade, gtk, mtl, random, regex-compat
, utf8-string, X11, X11-xft, xmonad, xmonad-contrib
}:

cabal.mkDerivation (self: {
  pname = "bluetile";
  version = "0.6";
  sha256 = "13xfnx08xgbfppr4cqmrqj82w192ll4m1x4kmv5jdpk02yb4zqa2";
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    ConfigFile filepath glade gtk mtl random regex-compat utf8-string X11
    X11-xft xmonad xmonad-contrib
  ];
  pkgconfigDepends = [ gtk ];
  doCheck = false;
  meta = {
    homepage = "http://www.bluetile.org/";
    description = "full-featured tiling for the GNOME desktop environment";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
