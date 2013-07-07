{ cabal, c2hs, ncursesw, panel, text, transformers }:

cabal.mkDerivation (self: {
  pname = "ncurses";
  version = "0.2.1";
  sha256 = "16cx4a2zr8irsp93rb5d4xxm4ky8slfw93z0j6p0rrgfsdhx7sp2";
  buildDepends = [ text transformers ];
  buildTools = [ c2hs ];
  extraLibraries = [ ncursesw panel ];
  doCheck = false;
  meta = {
    homepage = "https://john-millikin.com/software/haskell-ncurses/";
    description = "Modernised bindings to GNU ncurses";
    license = self.stdenv.lib.licenses.gpl3;
    platforms = self.ghc.meta.platforms;
  };
})
