{ cabal, augeas, HUnit }:

cabal.mkDerivation (self: {
  pname = "augeas";
  version = "0.6.1";
  sha256 = "08z6l97hi6clv3b34mz9zjc5rns02jx1zx9iqdsmjl2p7hcn7rs5";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [ HUnit ];
  extraLibraries = [ augeas ];
  pkgconfigDepends = [ augeas ];
  doCheck = false;
  meta = {
    homepage = "http://trac.haskell.org/augeas";
    description = "A Haskell FFI wrapper for the Augeas API";
    license = "LGPL";
    platforms = self.ghc.meta.platforms;
  };
})
