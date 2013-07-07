{ cabal, glib, tag_c, taglib_c }:

cabal.mkDerivation (self: {
  pname = "libtagc";
  version = "0.12.0";
  sha256 = "1f7r82cfrkxrqcrxk92y6zhk79qwpack2g67crww5q8hs7438vja";
  buildDepends = [ glib ];
  extraLibraries = [ tag_c ];
  pkgconfigDepends = [ taglib_c ];
  doCheck = false;
  meta = {
    homepage = "https://patch-tag.com/r/AndyStewart/libtagc/home";
    description = "Binding to TagLib C library";
    license = self.stdenv.lib.licenses.lgpl21;
    platforms = self.ghc.meta.platforms;
  };
})
