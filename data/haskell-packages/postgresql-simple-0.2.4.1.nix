{ cabal, attoparsec, base16-bytestring, blaze-builder, blaze-textual
, cryptohash, HUnit, postgresql-libpq, text, time, transformers, vector
}:

cabal.mkDerivation (self: {
  pname = "postgresql-simple";
  version = "0.2.4.1";
  sha256 = "09yszkiahfyidaq9yfk4mda5sf1m8bcqqag51vasybln9k9hhws3";
  buildDepends = [
    attoparsec blaze-builder blaze-textual postgresql-libpq text time
    transformers vector
  ];
  testDepends = [ base16-bytestring cryptohash HUnit text time ];
  doCheck = false;
  meta = {
    description = "Mid-Level PostgreSQL client library";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
