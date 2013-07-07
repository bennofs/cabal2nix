{ cabal, asn1-data, base64-bytestring, blaze-builder, blaze-builder-conduit
, case-insensitive, certificate, conduit, cookie, cprng-aes, data-default
, deepseq, failure, filepath, hspec, http-types, HUnit, lifted-base, mime-types
, monad-control, mtl, network, network-conduit, publicsuffixlist, random
, regex-compat, resourcet, socks, text, time, tls, tls-extra, transformers
, transformers-base, utf8-string, void, wai, warp, zlib-conduit
}:

cabal.mkDerivation (self: {
  pname = "http-conduit";
  version = "1.9.0";
  sha256 = "1b6f2sg96fddk5ywbfvbvj9z81695d35qymf31idpkyshcp9argk";
  buildDepends = [
    asn1-data base64-bytestring blaze-builder blaze-builder-conduit
    case-insensitive certificate conduit cookie cprng-aes data-default deepseq
    failure filepath http-types lifted-base mime-types monad-control mtl
    network publicsuffixlist random regex-compat resourcet socks text time tls
    tls-extra transformers transformers-base utf8-string void zlib-conduit
  ];
  testDepends = [
    asn1-data base64-bytestring blaze-builder blaze-builder-conduit
    case-insensitive certificate conduit cookie cprng-aes data-default deepseq
    failure filepath hspec http-types HUnit lifted-base mime-types
    monad-control mtl network network-conduit publicsuffixlist random
    regex-compat resourcet socks text time tls tls-extra transformers
    transformers-base utf8-string void wai warp zlib-conduit
  ];
  doCheck = false;
  meta = {
    homepage = "http://www.yesodweb.com/book/http-conduit";
    description = "HTTP client package with conduit interface and HTTPS support";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
