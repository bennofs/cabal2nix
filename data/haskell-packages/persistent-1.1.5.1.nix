{ cabal, aeson, attoparsec, base64-bytestring, blaze-html, blaze-markup
, conduit, hspec, lifted-base, monad-control, monad-logger, path-pieces
, pool-conduit, resourcet, silently, sqlite, text, time, transformers
, transformers-base, unordered-containers, vector
}:

cabal.mkDerivation (self: {
  pname = "persistent";
  version = "1.1.5.1";
  sha256 = "1ds0q3mzgg60makx7impyxsvpzd3issa89igcy176yb54ypjwfnz";
  buildDepends = [
    aeson attoparsec base64-bytestring blaze-html blaze-markup conduit
    lifted-base monad-control monad-logger path-pieces pool-conduit resourcet
    silently text time transformers transformers-base unordered-containers
    vector
  ];
  testDepends = [ hspec text ];
  extraLibraries = [ sqlite ];
  doCheck = false;
  meta = {
    homepage = "http://www.yesodweb.com/book/persistent";
    description = "Type-safe, multi-backend data serialization";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
