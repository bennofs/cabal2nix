{ cabal, filepath, mtl, regex-compat }:

cabal.mkDerivation (self: {
  pname = "lhs2tex";
  version = "1.18.1";
  sha256 = "0j4n7vkabsggn94gbwixy1vmckdck2nggdiqvk6n9nx164if5jnw";
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ filepath mtl regex-compat ];
  doCheck = false;
  meta = {
    homepage = "http://www.andres-loeh.de/lhs2tex/";
    description = "Preprocessor for typesetting Haskell sources with LaTeX";
    license = "GPL";
    platforms = self.ghc.meta.platforms;
  };
})
