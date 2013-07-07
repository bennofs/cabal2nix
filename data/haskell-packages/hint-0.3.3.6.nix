{ cabal, extensible-exceptions, filepath, ghc-mtl, ghc-paths, haskell-src
, MonadCatchIO-mtl, mtl, random, utf8-string
}:

cabal.mkDerivation (self: {
  pname = "hint";
  version = "0.3.3.6";
  sha256 = "080wnds99lg9p4n2h9d4bpgvk73yzc3im2ysn1r8f3nqai4b2can";
  buildDepends = [
    extensible-exceptions filepath ghc-mtl ghc-paths haskell-src
    MonadCatchIO-mtl mtl random utf8-string
  ];
  doCheck = false;
  meta = {
    homepage = "http://darcsden.com/jcpetruzza/hint";
    description = "Runtime Haskell interpreter (GHC API wrapper)";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
