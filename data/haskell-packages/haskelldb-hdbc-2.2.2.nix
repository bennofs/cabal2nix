{ cabal, convertible, haskelldb, HDBC, mtl }:

cabal.mkDerivation (self: {
  pname = "haskelldb-hdbc";
  version = "2.2.2";
  sha256 = "06vzzya6k1l74bm5ky7cbn6ch1wx5j5phnhi49bw17y3q632vd3f";
  buildDepends = [ convertible haskelldb HDBC mtl ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/m4dc4p/haskelldb";
    description = "HaskellDB support for HDBC";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
