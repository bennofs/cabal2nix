{ cabal, base-unicode-symbols, HUnit, test-framework, test-framework-hunit }:

cabal.mkDerivation (self: {
  pname = "terminal-progress-bar";
  version = "0.0.1.3";
  sha256 = "04glnz72ivnaqfhyxb5sc175yxiq7lf7xi1jhbk9gi95md00f1js";
  buildDepends = [ base-unicode-symbols ];
  testDepends = [
    base-unicode-symbols HUnit test-framework test-framework-hunit
  ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/roelvandijk/terminal-progress-bar";
    description = "A simple progress bar in the terminal";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
