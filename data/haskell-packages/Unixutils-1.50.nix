{ cabal, filepath, pureMD5, regex-tdfa, zlib }:

cabal.mkDerivation (self: {
  pname = "Unixutils";
  version = "1.50";
  sha256 = "0pw0x6nvl34c9l660g1j32xj4ay5szw2lp5yh8a1khphz1y0p3wd";
  buildDepends = [ filepath pureMD5 regex-tdfa zlib ];
  doCheck = false;
  meta = {
    homepage = "http://src.seereason.com/haskell-unixutils";
    description = "A crude interface between Haskell and Unix-like operating systems";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
