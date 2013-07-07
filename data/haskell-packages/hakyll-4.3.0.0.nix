{ cabal, binary, blaze-html, blaze-markup, citeproc-hs, cmdargs, cryptohash
, data-default, deepseq, filepath, fsnotify, http-conduit, http-types, HUnit
, lrucache, mtl, pandoc, parsec, QuickCheck, random, regex-base, regex-tdfa
, snap-core, snap-server, system-filepath, tagsoup, test-framework
, test-framework-hunit, test-framework-quickcheck2, text, time
}:

cabal.mkDerivation (self: {
  pname = "hakyll";
  version = "4.3.0.0";
  sha256 = "188j3spdi2mivx5a10whpb09fm8yhg54ddfwc6x0k040c7q3aq0q";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    binary blaze-html blaze-markup citeproc-hs cmdargs cryptohash data-default
    deepseq filepath fsnotify http-conduit http-types lrucache mtl pandoc
    parsec random regex-base regex-tdfa snap-core snap-server system-filepath
    tagsoup text time
  ];
  testDepends = [
    binary blaze-html blaze-markup citeproc-hs cmdargs cryptohash data-default
    deepseq filepath fsnotify http-conduit http-types HUnit lrucache mtl pandoc
    parsec QuickCheck random regex-base regex-tdfa snap-core snap-server
    system-filepath tagsoup test-framework test-framework-hunit
    test-framework-quickcheck2 text time
  ];
  doCheck = false;
  meta = {
    homepage = "http://jaspervdj.be/hakyll";
    description = "A static website compiler library";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
