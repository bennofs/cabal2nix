{ cabal, HaXml, mtl }:

cabal.mkDerivation (self: {
  pname = "darcs-monitor";
  version = "0.4.2";
  sha256 = "0rp6flaizbaxzr28fr82vaacl4wajh6zdqnwcbgyhwz5dj7rdanq";
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ HaXml mtl ];
  doCheck = false;
  meta = {
    homepage = "http://wiki.darcs.net/RelatedSoftware/DarcsMonitor";
    description = "Darcs repository monitor (sends email)";
    license = "GPL";
    platforms = self.ghc.meta.platforms;
  };
})
