{ cabal }:

cabal.mkDerivation (self: {
  pname = "utf8-string";
  version = "0.3.7";
  sha256 = "1s59xsw1i311rpxb7arnd280pjqab5mrlfjmxbabknka8wqlnnvq";
  doCheck = false;
  meta = {
    homepage = "http://github.com/glguy/utf8-string/";
    description = "Support for reading and writing UTF8 Strings";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
