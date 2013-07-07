{ cabal, blaze-builder, conduit, hspec, QuickCheck, text, transformers }:

cabal.mkDerivation (self: {
  pname = "blaze-builder-conduit";
  version = "1.0.0";
  sha256 = "15q0b0k5dxrp9cw2b4qf9mmjwz1mmxrn3890df5rc9z6yajajrc6";
  buildDepends = [ blaze-builder conduit text transformers ];
  testDepends = [ blaze-builder conduit hspec QuickCheck transformers ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/snoyberg/conduit";
    description = "Convert streams of builders to streams of bytestrings";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
