{ cabal, blaze-builder, byteorder, case-insensitive, date-cache, fast-logger
, http-types, network, wai
}:

cabal.mkDerivation (self: {
  pname = "wai-logger";
  version = "0.3.0";
  sha256 = "132jcjyqr7bxcfi7v9mapvx8dci0lz8rv91mgnrzgvpac542c2yq";
  buildDepends = [
    blaze-builder byteorder case-insensitive date-cache fast-logger http-types
    network wai
  ];
  doCheck = false;
  meta = {
    description = "A logging system for WAI";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
