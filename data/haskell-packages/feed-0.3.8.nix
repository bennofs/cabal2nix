{ cabal, utf8-string, xml }:

cabal.mkDerivation (self: {
  pname = "feed";
  version = "0.3.8";
  sha256 = "1yvigcvb8cvxfa8vb2i11xkrylqw57jwzkaji6m1wp03k80zf576";
  buildDepends = [ utf8-string xml ];
  doCheck = false;
  meta = {
    description = "Interfacing with RSS (v 0.9x, 2.x, 1.0) + Atom feeds.";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
