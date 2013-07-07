{ cabal, certificate, conduit, crypto-random-api, cryptocipher, network
, network-conduit, pem, tls, tls-extra, transformers, wai, warp
}:

cabal.mkDerivation (self: {
  pname = "warp-tls";
  version = "1.4.0.1";
  sha256 = "1h2yw2ygm39bdhm2c8xhs4lzjj9yj5avh0bpgc1wcbfsq9ql8za9";
  buildDepends = [
    certificate conduit crypto-random-api cryptocipher network network-conduit
    pem tls tls-extra transformers wai warp
  ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/yesodweb/wai";
    description = "SSL support for Warp via the TLS package";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
