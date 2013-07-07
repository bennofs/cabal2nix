{ cabal, base64-bytestring, blaze-html, monad-logger, mtl, persistent
, persistent-sqlite, pwstore-fast, random, resourcet, text, xml-conduit, yesod
, yesod-auth, yesod-core, yesod-form, yesod-persistent, yesod-test
}:

cabal.mkDerivation (self: {
  pname = "yesod-auth-account";
  version = "1.1.0.1";
  sha256 = "1qnhchl3pylfqki6n46cn3n5fsw5h8i8zx8xixa0f3lmclq1k8wz";
  buildDepends = [
    base64-bytestring blaze-html mtl persistent pwstore-fast random text
    yesod-auth yesod-core yesod-form yesod-persistent
  ];
  testDepends = [
    monad-logger mtl persistent-sqlite resourcet text xml-conduit yesod
    yesod-auth yesod-test
  ];
  doCheck = false;
  meta = {
    homepage = "https://bitbucket.org/wuzzeb/yesod-auth-account";
    description = "An account authentication plugin for Yesod";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
