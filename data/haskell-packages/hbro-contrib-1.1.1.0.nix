{ cabal, filepath, glib, gtk, hbro, monad-control, mtl, network, pango, text
, time, transformers-base, webkit
}:

cabal.mkDerivation (self: {
  pname = "hbro-contrib";
  version = "1.1.1.0";
  sha256 = "1j4ga3rzwiwr2k0vrqvn8p09d2gj3wzhdnlcfql6rzb8lx3kvw4l";
  buildDepends = [
    filepath glib gtk hbro monad-control mtl network pango text time
    transformers-base webkit
  ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/k0ral/hbro-contrib/";
    description = "Third-party extensions to hbro";
    license = "unknown";
    platforms = self.ghc.meta.platforms;
  };
})
