{ cabal, llvm-base, mtl, type-level }:

cabal.mkDerivation (self: {
  pname = "llvm";
  version = "3.2.0.0";
  sha256 = "133iqh4f3h1yzjmbkc4mky2pi31cjpf4wiwcb5qs6zn8xgz8z2yy";
  buildDepends = [ llvm-base mtl type-level ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/bos/llvm";
    description = "Bindings to the LLVM compiler toolkit";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
