{ cabal, blaze-builder, blaze-builder-conduit, conduit, http-types
, transformers, wai, warp, zlib-conduit
}:

cabal.mkDerivation (self: {
  pname = "wai-handler-launch";
  version = "1.3.1.4";
  sha256 = "0ii74p2400a4w0pcswk8j57wbasi17alifs4xgwv79b235wnn317";
  buildDepends = [
    blaze-builder blaze-builder-conduit conduit http-types transformers wai
    warp zlib-conduit
  ];
  doCheck = false;
  meta = {
    description = "Launch a web app in the default browser";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
