{ cabal, binary, blaze-builder, Cabal, case-insensitive, cmdargs, conduit
, filepath, haskell-src-exts, http-types, parsec, random, safe, tagsoup, time
, transformers, uniplate, wai, warp
}:

cabal.mkDerivation (self: {
  pname = "hoogle";
  version = "4.2.16";
  sha256 = "1hamwqhndrkajm4xvxxfhr2rnlmf3z2ysyfxx3y6d24jawjyqf8a";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    binary blaze-builder Cabal case-insensitive cmdargs conduit filepath
    haskell-src-exts http-types parsec random safe tagsoup time transformers
    uniplate wai warp
  ];
  doCheck = false;
  meta = {
    homepage = "http://www.haskell.org/hoogle/";
    description = "Haskell API Search";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
