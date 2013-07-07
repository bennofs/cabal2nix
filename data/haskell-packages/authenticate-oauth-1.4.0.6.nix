{ cabal, base64-bytestring, blaze-builder, blaze-builder-conduit, conduit
, crypto-pubkey-types, data-default, http-conduit, http-types, monad-control
, random, resourcet, RSA, SHA, time, transformers
}:

cabal.mkDerivation (self: {
  pname = "authenticate-oauth";
  version = "1.4.0.6";
  sha256 = "1ylfvc744wqyn5xbv6fivfys5kk9k9r2b9xf63zfzj5l5yqmv91a";
  buildDepends = [
    base64-bytestring blaze-builder blaze-builder-conduit conduit
    crypto-pubkey-types data-default http-conduit http-types monad-control
    random resourcet RSA SHA time transformers
  ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/yesodweb/authenticate";
    description = "Library to authenticate with OAuth for Haskell web applications";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
