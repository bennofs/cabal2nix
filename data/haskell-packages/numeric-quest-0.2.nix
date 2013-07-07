{ cabal }:

cabal.mkDerivation (self: {
  pname = "numeric-quest";
  version = "0.2";
  sha256 = "1x9yaknv7llhbz76qdy7msffvavcd7cj6am90q5pvf2g5vpn4d4y";
  doCheck = false;
  meta = {
    homepage = "http://www.haskell.org/haskellwiki/Numeric_Quest";
    description = "Math and quantum mechanics";
    license = "GPL";
    platforms = self.ghc.meta.platforms;
  };
})
