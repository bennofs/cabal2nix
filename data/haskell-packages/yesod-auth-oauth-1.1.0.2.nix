{ cabal, authenticate-oauth, lifted-base, text, transformers, yesod-auth
, yesod-core, yesod-form
}:

cabal.mkDerivation (self: {
  pname = "yesod-auth-oauth";
  version = "1.1.0.2";
  sha256 = "08i9zjmmwpvi87r0mhkl3qsqq09hrr9ycd9cq5ssn224g3qkblxl";
  buildDepends = [
    authenticate-oauth lifted-base text transformers yesod-auth yesod-core
    yesod-form
  ];
  doCheck = false;
  meta = {
    homepage = "http://www.yesodweb.com/";
    description = "OAuth Authentication for Yesod";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
