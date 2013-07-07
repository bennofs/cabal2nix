{ cabal, bindings-libzip, filepath, mtl, time }:

cabal.mkDerivation (self: {
  pname = "LibZip";
  version = "0.10.2";
  sha256 = "0flpmgi07qfbmsd2hplhrvzdkby065ly4a5yrvbsrx6p7gkivn2l";
  buildDepends = [ bindings-libzip filepath mtl time ];
  doCheck = false;
  meta = {
    homepage = "http://bitbucket.org/astanin/hs-libzip/";
    description = "Bindings to libzip, a library for manipulating zip archives";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
