{ cabal, utf8-string }:

cabal.mkDerivation (self: {
  pname = "url";
  version = "2.1.2";
  sha256 = "14gg8dks3zyclnws0dsdrj116yw9czc6dpp41aaf7bqqchlw9x9c";
  buildDepends = [ utf8-string ];
  doCheck = false;
  meta = {
    homepage = "http://www.haskell.org/haskellwiki/Url";
    description = "A library for working with URLs";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
