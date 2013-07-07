{ cabal, date-cache, fast-logger, http-types, wai, wai-logger }:

cabal.mkDerivation (self: {
  pname = "wai-logger-prefork";
  version = "0.3.0";
  sha256 = "0cfslqr2zdj0x83dbscafhdljrn2xswym7hpf23zlrsrnpz71qy4";
  buildDepends = [ date-cache fast-logger http-types wai wai-logger ];
  doCheck = false;
  meta = {
    description = "A logging system for preforked WAI apps";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
