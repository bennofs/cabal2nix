{ cabal, libsndfile, openal, pthread }:

cabal.mkDerivation (self: {
  pname = "sfml-audio";
  version = "0.7.0.1816";
  sha256 = "1799nvv7gx841hp23x7gjdqlsxw9pwb798yip339cwj0saj999nh";
  extraLibraries = [ libsndfile openal pthread ];
  doCheck = false;
  meta = {
    homepage = "http://patch-tag.com/r/shahn/sfml-audio";
    description = "minimal bindings to the audio module of sfml";
    license = "unknown";
    platforms = self.ghc.meta.platforms;
  };
})
