{ cabal, aeson, conduit, monad-control, monad-logger, persistent, text
, transformers
}:

cabal.mkDerivation (self: {
  pname = "persistent-sqlite";
  version = "1.1.5";
  sha256 = "1kqxp0qsvxfz352yg9k1wgb2nfm7kcz8xdj9q537ilh99wxkp6cl";
  buildDepends = [
    aeson conduit monad-control monad-logger persistent text transformers
  ];
  doCheck = false;
  meta = {
    homepage = "http://www.yesodweb.com/book/persistent";
    description = "Backend for the persistent library using sqlite3";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
