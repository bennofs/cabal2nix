{ cabal, base64-bytestring, blaze-builder, filepath, random, text }:

cabal.mkDerivation (self: {
  pname = "mime-mail";
  version = "0.4.1.2";
  sha256 = "01dw9zvgxmwg0jslw14a9kjrmyjvwla8bw40w2426ifdwwxb3ywy";
  buildDepends = [ base64-bytestring blaze-builder filepath random text ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/snoyberg/mime-mail";
    description = "Compose MIME email messages";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
