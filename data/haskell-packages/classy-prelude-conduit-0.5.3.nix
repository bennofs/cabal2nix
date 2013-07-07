{ cabal, classy-prelude, conduit, hspec, monad-control, QuickCheck, resourcet
, transformers, void, xml-conduit
}:

cabal.mkDerivation (self: {
  pname = "classy-prelude-conduit";
  version = "0.5.3";
  sha256 = "1rmx439kdjipyz2s3v2s1xv1mb55kb4njl9k6f8mfhykgac39rhz";
  buildDepends = [
    classy-prelude conduit monad-control resourcet transformers void
    xml-conduit
  ];
  testDepends = [ conduit hspec QuickCheck transformers ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/snoyberg/classy-prelude";
    description = "conduit instances for classy-prelude";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
