{ cabal, deepseq, hashable, HUnit, test-framework, test-framework-hunit, text
}:

cabal.mkDerivation (self: {
  pname = "case-insensitive";
  version = "1.0.0.1";
  sha256 = "1yp8895qvxcmai0hvxdq77qss9cia2f9fyn6rm0hln3rcx8n53xm";
  buildDepends = [ deepseq hashable text ];
  testDepends = [ HUnit test-framework test-framework-hunit text ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/basvandijk/case-insensitive";
    description = "Case insensitive string comparison";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
