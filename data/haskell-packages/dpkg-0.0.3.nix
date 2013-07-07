{ cabal, bindings-DSL, HUnit, libdpkg, monad-loops }:

cabal.mkDerivation (self: {
  pname = "dpkg";
  version = "0.0.3";
  sha256 = "1bqrj1vqqjnv3qcs1s7lbwyzry95fzxrhi6340zqv0ibvyqnaz5k";
  buildDepends = [ bindings-DSL monad-loops ];
  testDepends = [ HUnit libdpkg ];
  pkgconfigDepends = [ libdpkg ];
  doCheck = false;
  meta = {
    description = "libdpkg bindings";
    license = self.stdenv.lib.licenses.gpl3;
    platforms = self.ghc.meta.platforms;
  };
})
