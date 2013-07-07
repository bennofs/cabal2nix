{ cabal, ansi-terminal, doctest, filepath, ghc-paths, hspec-expectations
, hspec-meta, HUnit, QuickCheck, setenv, silently, time, transformers
}:

cabal.mkDerivation (self: {
  pname = "hspec";
  version = "1.4.4";
  sha256 = "09wrvdlqzpa3vjcnirnzpj8nsvqnn5xbilnxaqmjm2agbl8xfj5r";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    ansi-terminal filepath hspec-expectations HUnit QuickCheck setenv silently
    time transformers
  ];
  testDepends = [
    ansi-terminal doctest filepath ghc-paths hspec-expectations hspec-meta
    HUnit QuickCheck setenv silently time transformers
  ];
  doCheck = false;
  meta = {
    homepage = "http://hspec.github.com/";
    description = "Behavior-Driven Development for Haskell";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
