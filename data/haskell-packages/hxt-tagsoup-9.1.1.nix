{ cabal, hxt, hxt-charproperties, hxt-unicode, tagsoup }:

cabal.mkDerivation (self: {
  pname = "hxt-tagsoup";
  version = "9.1.1";
  sha256 = "06w7gi8dm3rwb8wd47wmvz69z349968nr4ji14siv82slxf7r616";
  buildDepends = [ hxt hxt-charproperties hxt-unicode tagsoup ];
  doCheck = false;
  meta = {
    homepage = "http://www.fh-wedel.de/~si/HXmlToolbox/index.html";
    description = "TagSoup parser for HXT";
    license = "unknown";
    platforms = self.ghc.meta.platforms;
  };
})
