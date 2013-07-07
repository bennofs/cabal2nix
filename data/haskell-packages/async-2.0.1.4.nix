{ cabal, HUnit, stm, test-framework, test-framework-hunit }:

cabal.mkDerivation (self: {
  pname = "async";
  version = "2.0.1.4";
  sha256 = "1hi40bjwpl65mz7zj0sgh16bp9dwafbm5ysi2q8fzwwq5l0zxpa1";
  buildDepends = [ stm ];
  testDepends = [ HUnit test-framework test-framework-hunit ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/simonmar/async";
    description = "Run IO operations asynchronously and wait for their results";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
