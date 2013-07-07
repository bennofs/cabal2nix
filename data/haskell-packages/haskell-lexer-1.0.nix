{ cabal }:

cabal.mkDerivation (self: {
  pname = "haskell-lexer";
  version = "1.0";
  sha256 = "11jv30msg4rym73lk6j21ky2510c3av8qxk5p4z63j4m283w9l46";
  doCheck = false;
  meta = {
    description = "A fully compliant Haskell 98 lexer";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
