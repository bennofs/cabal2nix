{ cabal, Cabal, hledger-lib, mtl, time }:

cabal.mkDerivation (self: {
  pname = "hledger-interest";
  version = "1.4.2";
  sha256 = "1his6pxrvs5p28bmk77bd8vaq6rhjlilwq598mbkgfvlqg7q076v";
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ Cabal hledger-lib mtl time ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/peti/hledger-interest";
    description = "computes interest for a given account";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
