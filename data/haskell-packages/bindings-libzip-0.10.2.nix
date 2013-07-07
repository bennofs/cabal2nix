{ cabal, bindings-DSL, libzip }:

cabal.mkDerivation (self: {
  pname = "bindings-libzip";
  version = "0.10.2";
  sha256 = "15s89jhl748cgay81wi8l85brn9znjc10achsf0vyrpkjgdca8p7";
  buildDepends = [ bindings-DSL ];
  pkgconfigDepends = [ libzip ];
  doCheck = false;
  meta = {
    homepage = "http://bitbucket.org/astanin/hs-libzip/";
    description = "Low level bindings to libzip";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
