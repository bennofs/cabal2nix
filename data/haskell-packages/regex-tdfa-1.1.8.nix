{ cabal, mtl, parsec, regex-base }:

cabal.mkDerivation (self: {
  pname = "regex-tdfa";
  version = "1.1.8";
  sha256 = "1m75xh5bwmmgg5f757dc126kv47yfqqnz9fzj1hc80p6jpzs573x";
  buildDepends = [ mtl parsec regex-base ];
  doCheck = false;
  meta = {
    homepage = "http://hackage.haskell.org/package/regex-tdfa";
    description = "Replaces/Enhances Text.Regex";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
