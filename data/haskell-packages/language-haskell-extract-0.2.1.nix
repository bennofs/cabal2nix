{ cabal, haskell-src-exts, regex-posix }:

cabal.mkDerivation (self: {
  pname = "language-haskell-extract";
  version = "0.2.1";
  sha256 = "0lmg16g3z8cx0vb037bk4j2nr3qvybfcqfsr8l6jk57b2nz3yhbf";
  buildDepends = [ haskell-src-exts regex-posix ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/finnsson/template-helper";
    description = "Module to automatically extract functions from the local code";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
