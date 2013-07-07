{ cabal, HTTP, network }:

cabal.mkDerivation (self: {
  pname = "TinyURL";
  version = "0.1.0";
  sha256 = "0y8bl6w3ix2zjhm10wazgi70sr02ydc3hrwjbr6whk341n140wsh";
  buildDepends = [ HTTP network ];
  doCheck = false;
  meta = {
    description = "Use TinyURL to compress URLs";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
