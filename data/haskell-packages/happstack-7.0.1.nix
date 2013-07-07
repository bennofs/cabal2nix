{ cabal, happstack-server }:

cabal.mkDerivation (self: {
  pname = "happstack";
  version = "7.0.1";
  sha256 = "12qjwalq09kbvnk8cz7nmr6p53zx264ld8860spp3icif8wig9bx";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [ happstack-server ];
  doCheck = false;
  meta = {
    homepage = "http://happstack.com";
    description = "The haskell application server stack + code generation";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
