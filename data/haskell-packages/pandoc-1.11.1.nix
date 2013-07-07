{ cabal, ansi-terminal, base64-bytestring, blaze-html, blaze-markup
, citeproc-hs, data-default, Diff, extensible-exceptions, filepath
, highlighting-kate, HTTP, HUnit, json, mtl, network, pandoc-types, parsec
, QuickCheck, random, syb, tagsoup, temporary, test-framework
, test-framework-hunit, test-framework-quickcheck2, texmath, text, time, xml
, zip-archive, zlib
}:

cabal.mkDerivation (self: {
  pname = "pandoc";
  version = "1.11.1";
  sha256 = "0b23vrgkm1csykx1zrldkg5ka816j6m7a5fhs4cxffalifq91c7b";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    base64-bytestring blaze-html blaze-markup citeproc-hs data-default
    extensible-exceptions filepath highlighting-kate HTTP json mtl network
    pandoc-types parsec random syb tagsoup temporary texmath text time xml
    zip-archive zlib
  ];
  testDepends = [
    ansi-terminal Diff filepath highlighting-kate HUnit pandoc-types QuickCheck
    syb test-framework test-framework-hunit test-framework-quickcheck2 text
  ];
  configureFlags = "-fblaze_html_0_5";
  doCheck = false;
  meta = {
    homepage = "http://johnmacfarlane.net/pandoc";
    description = "Conversion between markup formats";
    license = "GPL";
    platforms = self.ghc.meta.platforms;
  };
})
