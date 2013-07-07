{ cabal, base64-bytestring, base64-conduit, classy-prelude, conduit, hspec, mtl
, QuickCheck, resourcet, system-fileio, system-filepath, text, transformers
}:

cabal.mkDerivation (self: {
  pname = "project-template";
  version = "0.1.3";
  sha256 = "1nsc4i3s9a7d0nysswhylvnr79z0ax9biiwr7b6jn7jzx0f2bdmm";
  buildDepends = [
    base64-bytestring base64-conduit classy-prelude conduit mtl resourcet
    system-fileio system-filepath text transformers
  ];
  testDepends = [
    base64-bytestring classy-prelude conduit hspec QuickCheck transformers
  ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/fpco/haskell-ide";
    description = "Specify Haskell project templates and generate files";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
