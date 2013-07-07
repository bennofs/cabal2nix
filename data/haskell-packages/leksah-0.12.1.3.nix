{ cabal, binary, binary-shared, Cabal, deepseq, enumerator, filepath, gio, glib
, gtk, gtksourceview2, hslogger, leksah-server, ltk, mtl, network, parsec
, QuickCheck, regex-base, regex-tdfa, strict, text, time, transformers
, utf8-string
}:

cabal.mkDerivation (self: {
  pname = "leksah";
  version = "0.12.1.3";
  sha256 = "1w61wnd0nq9iqh0pwn9dz3m4qn3m0zasv0m3ki4k7plcdrmkqb3y";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    binary binary-shared Cabal deepseq enumerator filepath gio glib gtk
    gtksourceview2 hslogger leksah-server ltk mtl network parsec QuickCheck
    regex-base regex-tdfa strict text time transformers utf8-string
  ];
  testDepends = [ Cabal QuickCheck ];
  doCheck = false;
  meta = {
    homepage = "http://www.leksah.org";
    description = "Haskell IDE written in Haskell";
    license = "GPL";
    platforms = self.ghc.meta.platforms;
  };
})
