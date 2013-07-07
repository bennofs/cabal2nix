{ cabal, aeson, conduit, monad-control, persistent, postgresql-libpq
, postgresql-simple, text, time, transformers
}:

cabal.mkDerivation (self: {
  pname = "persistent-postgresql";
  version = "1.1.3.1";
  sha256 = "1lbr4x45hzk73439x6is9zw6y3mqy1ivmblncvg70kzw0kmjhpnk";
  buildDepends = [
    aeson conduit monad-control persistent postgresql-libpq postgresql-simple
    text time transformers
  ];
  doCheck = false;
  meta = {
    homepage = "http://www.yesodweb.com/book/persistent";
    description = "Backend for the persistent library using postgresql";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
