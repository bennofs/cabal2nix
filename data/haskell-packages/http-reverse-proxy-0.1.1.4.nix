{ cabal, blaze-builder, case-insensitive, classy-prelude, conduit, data-default
, hspec, http-conduit, http-types, lifted-base, monad-control, network
, network-conduit, text, transformers, wai, warp, word8
}:

cabal.mkDerivation (self: {
  pname = "http-reverse-proxy";
  version = "0.1.1.4";
  sha256 = "0j77hp1ddbxrsv65xf6kqbl8jnvl6qzx98p0lg73j7s76j7vg9cd";
  buildDepends = [
    blaze-builder case-insensitive classy-prelude conduit data-default
    http-conduit http-types lifted-base monad-control network network-conduit
    text wai warp word8
  ];
  testDepends = [
    blaze-builder conduit hspec http-conduit http-types lifted-base network
    network-conduit transformers wai warp
  ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/fpco/http-reverse-proxy";
    description = "Reverse proxy HTTP requests, either over raw sockets or with WAI";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
