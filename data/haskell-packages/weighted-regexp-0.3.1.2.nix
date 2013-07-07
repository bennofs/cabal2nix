{ cabal, happy }:

cabal.mkDerivation (self: {
  pname = "weighted-regexp";
  version = "0.3.1.2";
  sha256 = "0r765ppzazdsm5i3prgf6a405f88xi8sx79jdz9mck4584w7fqzr";
  isLibrary = true;
  isExecutable = true;
  buildTools = [ happy ];
  doCheck = false;
  meta = {
    homepage = "http://sebfisch.github.com/haskell-regexp";
    description = "Weighted Regular Expression Matcher";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
