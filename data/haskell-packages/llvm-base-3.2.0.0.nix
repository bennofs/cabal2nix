{ cabal, llvm, mtl }:

cabal.mkDerivation (self: {
  pname = "llvm-base";
  version = "3.2.0.0";
  sha256 = "1mm7bimnx2b1nlc20ra6v6hqsqspadzrs8v476msk8mbcf8lbmsv";
  buildDepends = [ mtl ];
  extraLibraries = [ llvm ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/bos/llvm";
    description = "FFI bindings to the LLVM compiler toolkit";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
