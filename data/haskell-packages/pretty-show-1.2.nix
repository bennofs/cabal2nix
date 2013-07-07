{ cabal, haskell-lexer }:

cabal.mkDerivation (self: {
  pname = "pretty-show";
  version = "1.2";
  sha256 = "0lbalmyrqisgd2spbvzifsy25lr6cl9sgz78hav8q8r406k7nf2l";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [ haskell-lexer ];
  doCheck = false;
  meta = {
    homepage = "http://wiki.github.com/yav/pretty-show";
    description = "Tools for working with derived Show instances";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
