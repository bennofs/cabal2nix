{ cabal, blaze-builder, blaze-html, blaze-markup, failure, hspec, HUnit, parsec
, shakespeare, text
}:

cabal.mkDerivation (self: {
  pname = "hamlet";
  version = "1.1.7.1";
  sha256 = "1kcjzvslxis8qapy7g1cqkqqamrnkyammy33pan7ckzv5fd59q5p";
  buildDepends = [
    blaze-builder blaze-html blaze-markup failure parsec shakespeare text
  ];
  testDepends = [ blaze-html blaze-markup hspec HUnit parsec text ];
  doCheck = false;
  meta = {
    homepage = "http://www.yesodweb.com/book/shakespearean-templates";
    description = "Haml-like template files that are compile-time checked";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
