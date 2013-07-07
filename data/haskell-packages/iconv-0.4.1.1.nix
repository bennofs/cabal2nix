{ cabal }:

cabal.mkDerivation (self: {
  pname = "iconv";
  version = "0.4.1.1";
  sha256 = "0kwq82qhq67qg7i137ipjsdapxq8bypkf0bq7a3qfpk1n414v20s";
  doCheck = false;
  meta = {
    description = "String encoding conversion";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
