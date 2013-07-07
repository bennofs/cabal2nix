{ cabal, crypto-api, mtl, tagged, transformers }:

cabal.mkDerivation (self: {
  pname = "monadcryptorandom";
  version = "0.5";
  sha256 = "19sb9mgfvmmmx1sn0aiq6dn5nl0201xwgs6xqakn7pz5vsqcww2b";
  buildDepends = [ crypto-api mtl tagged transformers ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/TomMD/monadcryptorandom";
    description = "A monad for using CryptoRandomGen";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
