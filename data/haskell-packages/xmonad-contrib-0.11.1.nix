{ cabal, extensible-exceptions, mtl, random, utf8-string, X11, X11-xft, xmonad
}:

cabal.mkDerivation (self: {
  pname = "xmonad-contrib";
  version = "0.11.1";
  sha256 = "1356dn8ipw7fgn2xijppn69f64zg36gcxbw9qndxbbmml5gq0zrl";
  buildDepends = [
    extensible-exceptions mtl random utf8-string X11 X11-xft xmonad
  ];
  doCheck = false;
  meta = {
    homepage = "http://xmonad.org/";
    description = "Third party extensions for xmonad";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
