{ cabal, blaze-html, blaze-markup, reform, text }:

cabal.mkDerivation (self: {
  pname = "reform-blaze";
  version = "0.1.2";
  sha256 = "04228r3avg8k3jh49kp3ixbhf5x6h5ssz0gh4cxs7ya1q9lfq3v0";
  buildDepends = [ blaze-html blaze-markup reform text ];
  doCheck = false;
  meta = {
    homepage = "http://www.happstack.com/";
    description = "Add support for using blaze-html with Reform";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
