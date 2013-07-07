{ cabal, deepseq, hashable }:

cabal.mkDerivation (self: {
  pname = "hashmap";
  version = "1.3.0.1";
  sha256 = "16scl1rbq0f18mggrj4lbhgkz7kzshsw9a523rjd8cjkdjz1ijwr";
  buildDepends = [ deepseq hashable ];
  doCheck = false;
  meta = {
    homepage = "http://git.auryn.cz/haskell/hashmap/";
    description = "Persistent containers Map and Set based on hashing";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
