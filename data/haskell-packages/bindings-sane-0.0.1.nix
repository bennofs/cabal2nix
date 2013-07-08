{ cabal, bindings-DSL, saneBackends }:

cabal.mkDerivation (self: {
  pname = "bindings-sane";
  version = "0.0.1";
  sha256 = "0jxhc0x5hq6y7iznqlxbgnl37a7k8yki2ri475gyc158d47b0zm2";
  buildDepends = [ bindings-DSL ];
  pkgconfigDepends = [ saneBackends ];
  doCheck = false;
  meta = {
    homepage = "http://floss.scru.org/bindings-sane";
    description = "FFI bindings to libsane";
    license = self.stdenv.lib.licenses.gpl3;
    platforms = self.ghc.meta.platforms;
  };
})
