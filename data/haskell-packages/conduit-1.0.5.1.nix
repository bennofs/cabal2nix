{ cabal, doctest, hspec, lifted-base, mmorph, monad-control, QuickCheck
, resourcet, text, transformers, transformers-base, void
}:

cabal.mkDerivation (self: {
  pname = "conduit";
  version = "1.0.5.1";
  sha256 = "09ppvvjw6jfyg760wqbgqg403018vpm1hynkgsvswb5fgsfshs0d";
  buildDepends = [
    lifted-base mmorph monad-control resourcet text transformers
    transformers-base void
  ];
  testDepends = [ doctest hspec QuickCheck resourcet text transformers void ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/snoyberg/conduit";
    description = "Streaming data processing library";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
