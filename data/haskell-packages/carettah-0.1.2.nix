{ cabal, cairo, filepath, gtk, hcwiid, highlighting-kate, mtl, pandoc, pango
, time
}:

cabal.mkDerivation (self: {
  pname = "carettah";
  version = "0.1.2";
  sha256 = "1p1zq81b5np6iw0az618zcpva1yr5wwxd89b70d7h25f2pyq9k8c";
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    cairo filepath gtk hcwiid highlighting-kate mtl pandoc pango time
  ];
  doCheck = false;
  meta = {
    homepage = "http://carettah.masterq.net/";
    description = "A presentation tool writtten with Haskell";
    license = self.stdenv.lib.licenses.gpl2;
    platforms = self.ghc.meta.platforms;
  };
})
