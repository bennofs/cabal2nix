{ cabal, base-compat, deepseq, filepath, ghc-paths, hspec, HUnit, QuickCheck
, setenv, silently, stringbuilder, syb, transformers
}:

cabal.mkDerivation (self: {
  pname = "doctest";
  version = "0.9.6";
  sha256 = "0gw13pm4hg69v60swsv6w4iwzgdj5f4pkcyfmgzfp1dx399p6hyl";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [ deepseq filepath ghc-paths syb transformers ];
  testDepends = [
    base-compat deepseq filepath ghc-paths hspec HUnit QuickCheck setenv
    silently stringbuilder syb transformers
  ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/sol/doctest-haskell#readme";
    description = "Test interactive Haskell examples";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
