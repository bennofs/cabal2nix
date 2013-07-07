{ cabal, attoparsec, attoparsec-conduit, blaze-builder, blaze-builder-conduit
, case-insensitive, conduit, hspec, HUnit, QuickCheck, text, transformers
}:

cabal.mkDerivation (self: {
  pname = "tagstream-conduit";
  version = "0.5.4";
  sha256 = "1djf66kn3m4sdwmis82f9w2nkmjyrq12zda7ic9pcsvra579868i";
  buildDepends = [
    attoparsec attoparsec-conduit blaze-builder blaze-builder-conduit
    case-insensitive conduit text transformers
  ];
  testDepends = [ conduit hspec HUnit QuickCheck text ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/yihuang/tagstream-conduit";
    description = "streamlined html tag parser";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
