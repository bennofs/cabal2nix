{ cabal, persistent, persistent-template, transformers, yesod-core }:

cabal.mkDerivation (self: {
  pname = "yesod-persistent";
  version = "1.1.0.1";
  sha256 = "0kgd1b7kaif644hjbvkc53yxr7qk310zdndypd9h0j31paw52k1p";
  buildDepends = [ persistent persistent-template transformers yesod-core ];
  doCheck = false;
  meta = {
    homepage = "http://www.yesodweb.com/";
    description = "Some helpers for using Persistent from Yesod";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
