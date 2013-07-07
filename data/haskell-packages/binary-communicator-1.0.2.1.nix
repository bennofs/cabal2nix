{ cabal, binary, mtl }:

cabal.mkDerivation (self: {
  pname = "binary-communicator";
  version = "1.0.2.1";
  sha256 = "02w5ybp5fdqwz1ffvfs3pfrpx67bzh75njgzr6iai1vrdyjisfkl";
  buildDepends = [ binary mtl ];
  doCheck = false;
  meta = {
    description = "Flexible way to ease transmission of binary data";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
