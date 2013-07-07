{ cabal, cond, data-default, dyre, filepath, glib, gtk, lens, monad-control
, mtl, network, pango, transformers, transformers-base, webkit, xdg-basedir
, zeromq3-haskell
}:

cabal.mkDerivation (self: {
  pname = "hbro";
  version = "1.1.2.0";
  sha256 = "03i876ynf33j724521smbr14cz7mx64qlfcq5ar0i3k2vcllk3a6";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    cond data-default dyre filepath glib gtk lens monad-control mtl network
    pango transformers transformers-base webkit xdg-basedir zeromq3-haskell
  ];
  doCheck = false;
  meta = {
    homepage = "http://projects.haskell.org/hbro/";
    description = "Minimal KISS compliant browser";
    license = "unknown";
    platforms = self.ghc.meta.platforms;
  };
})
