{ cabal, lifted-base, transformers }:

cabal.mkDerivation (self: {
  pname = "file-location";
  version = "0.4.5.1";
  sha256 = "1k8z2hjm3g9127kcy7gzmpnd4nxb1dy1j4h3v6n2dxsvh979k2v8";
  buildDepends = [ lifted-base transformers ];
  testDepends = [ lifted-base transformers ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/gregwebs/FileLocation.hs";
    description = "common functions that show file location information";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
