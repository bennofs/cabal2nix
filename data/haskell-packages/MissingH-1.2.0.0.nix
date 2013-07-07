{ cabal, filepath, hslogger, HUnit, mtl, network, parsec, random, regex-compat
, time
}:

cabal.mkDerivation (self: {
  pname = "MissingH";
  version = "1.2.0.0";
  sha256 = "0bqg1j2pvm0ixrbnsxrr5kgibhbp191irhcavqlwfwgaxhrpqnm1";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    filepath hslogger HUnit mtl network parsec random regex-compat time
  ];
  doCheck = false;
  meta = {
    homepage = "http://software.complete.org/missingh";
    description = "Large utility library";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
