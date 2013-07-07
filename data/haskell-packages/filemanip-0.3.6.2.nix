{ cabal, filepath, mtl, unix-compat }:

cabal.mkDerivation (self: {
  pname = "filemanip";
  version = "0.3.6.2";
  sha256 = "03l114rhb4f6nyzs9w14i79d7kyyq9ia542alsqpbmikm9gxm4rz";
  buildDepends = [ filepath mtl unix-compat ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/bos/filemanip";
    description = "Expressive file and directory manipulation for Haskell";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
