{ cabal, monads-tf, system-filepath, text, transformers }:

cabal.mkDerivation (self: {
  pname = "options";
  version = "0.1.1";
  sha256 = "1nr8szfwis2ppqyll2df3cn4bci4raca2mmywnkzlh9rxm4r2ci9";
  buildDepends = [ monads-tf system-filepath text transformers ];
  doCheck = false;
  meta = {
    homepage = "https://john-millikin.com/software/haskell-options/";
    description = "Parsing command-line options";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
