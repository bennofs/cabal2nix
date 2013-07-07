{ cabal, binary, filepath, ghc-paths, io-storage, time, xdg-basedir }:

cabal.mkDerivation (self: {
  pname = "dyre";
  version = "0.8.9";
  sha256 = "1gyiakbbkg01a8ymva9nj0adv9skprlp767nhc81nn7lwblxn06i";
  buildDepends = [ binary filepath ghc-paths io-storage time xdg-basedir ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/willdonnelly/dyre";
    description = "Dynamic reconfiguration in Haskell";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
