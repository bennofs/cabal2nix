{ cabal, attoparsec, attoparsec-enumerator, blaze-builder
, blaze-builder-enumerator, bytestring-mmap, case-insensitive, deepseq
, enumerator, filepath, HUnit, MonadCatchIO-transformers, mtl, random
, regex-posix, text, time, unix-compat, unordered-containers, vector, zlib-enum
}:

cabal.mkDerivation (self: {
  pname = "snap-core";
  version = "0.9.3.1";
  sha256 = "1q2lk70l0hk4l6ksjnal1bfkby0i08gdzvj9cscvxs4njxmgdapq";
  buildDepends = [
    attoparsec attoparsec-enumerator blaze-builder blaze-builder-enumerator
    bytestring-mmap case-insensitive deepseq enumerator filepath HUnit
    MonadCatchIO-transformers mtl random regex-posix text time unix-compat
    unordered-containers vector zlib-enum
  ];
  doCheck = false;
  meta = {
    homepage = "http://snapframework.com/";
    description = "Snap: A Haskell Web Framework (core interfaces and types)";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
