{ cabal, base64-bytestring, blaze-html, cgi, ConfigFile, feed, filepath
, filestore, ghc-paths, happstack-server, highlighting-kate, hslogger
, HStringTemplate, HTTP, json, mtl, network, pandoc, pandoc-types, parsec
, random, recaptcha, safe, SHA, syb, tagsoup, text, time, url, utf8-string
, xhtml, xml, xss-sanitize, zlib
}:

cabal.mkDerivation (self: {
  pname = "gitit";
  version = "0.10.3.1";
  sha256 = "1sm6rryfyqr0nd4flbc5d520xyw2ajnkylvqf4fi4dhl6fnbpam5";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    base64-bytestring blaze-html cgi ConfigFile feed filepath filestore
    ghc-paths happstack-server highlighting-kate hslogger HStringTemplate HTTP
    json mtl network pandoc pandoc-types parsec random recaptcha safe SHA syb
    tagsoup text time url utf8-string xhtml xml xss-sanitize zlib
  ];
  doCheck = false;
  meta = {
    homepage = "http://gitit.net";
    description = "Wiki using happstack, git or darcs, and pandoc";
    license = "GPL";
    platforms = self.ghc.meta.platforms;
  };
})
