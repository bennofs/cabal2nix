{ cabal, HTTP, network, xhtml }:

cabal.mkDerivation (self: {
  pname = "recaptcha";
  version = "0.1";
  sha256 = "0i2iyahwqd9728ph7lygp70mjbg0xc9v9r3c0g6s36fxmbiyc06y";
  buildDepends = [ HTTP network xhtml ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/jgm/recaptcha/tree/master";
    description = "Functions for using the reCAPTCHA service in web applications";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
