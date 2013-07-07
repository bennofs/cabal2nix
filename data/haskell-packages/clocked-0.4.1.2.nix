{ cabal, clock, MonadCatchIO-transformers, QtCore, transformers }:

cabal.mkDerivation (self: {
  pname = "clocked";
  version = "0.4.1.2";
  sha256 = "1mvj2fwgw434p79l97bp4rzxbk5abhn44b2knz29wkx2msq5llng";
  buildDepends = [ clock MonadCatchIO-transformers transformers ];
  pkgconfigDepends = [ QtCore ];
  doCheck = false;
  meta = {
    homepage = "http://patch-tag.com/r/shahn/clocked/home";
    description = "timer functionality to clock IO commands";
    license = self.stdenv.lib.licenses.gpl3;
    platforms = self.ghc.meta.platforms;
  };
})
