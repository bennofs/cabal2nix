{ cabal, convertible, mtl, text, time, utf8-string }:

cabal.mkDerivation (self: {
  pname = "HDBC";
  version = "2.3.1.2";
  sha256 = "1rjamh8xscb9jhxgxcrs2qnvs2ipv9dqgnn0bpv5vwclmzmn5j87";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [ convertible mtl text time utf8-string ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/hdbc/hdbc";
    description = "Haskell Database Connectivity";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
