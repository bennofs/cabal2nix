{ cabal, regex-base, regex-tdfa, utf8-string }:

cabal.mkDerivation (self: {
  pname = "regex-tdfa-utf8";
  version = "1.0";
  sha256 = "0i5di03v9dsvvhz8mdfx5qba8zcpim0fpx1cjg9gvz4gh0yhqf4k";
  buildDepends = [ regex-base regex-tdfa utf8-string ];
  doCheck = false;
  meta = {
    description = "This combines regex-tdfa with utf8-string to allow searching over UTF8 encoded lazy bytestrings";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
