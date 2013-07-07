{ cabal, fuse }:

cabal.mkDerivation (self: {
  pname = "HFuse";
  version = "0.2.4.1";
  sha256 = "12k04dvh92kk2i68bcb70xnk378qxmh46f241k06di5rkcgwyg1k";
  extraLibraries = [ fuse ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/toothbrush/hfuse";
    description = "HFuse is a binding for the Linux FUSE library";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
