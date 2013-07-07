{ cabal, Cabal, debian, filepath, mtl, parsec, regex-tdfa, Unixutils
, utf8-string
}:

cabal.mkDerivation (self: {
  pname = "cabal-debian";
  version = "1.25";
  sha256 = "10k351xwk55qa2sxy3qiz7c55lbrw8985mbz3vp7y3yfpx75d3ah";
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    Cabal debian filepath mtl parsec regex-tdfa Unixutils utf8-string
  ];
  doCheck = false;
  meta = {
    homepage = "http://src.seereason.com/cabal-debian";
    description = "Create a debianization for a cabal package";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
