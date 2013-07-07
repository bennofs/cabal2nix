{ cabal, ansi-wl-pprint, bzlib, Cabal, filepath, HaXml, HUnit, mtl, network
, parsec, process-extras, pureMD5, regex-compat, regex-tdfa, text, time
, Unixutils, utf8-string, zlib
}:

cabal.mkDerivation (self: {
  pname = "debian";
  version = "3.70.1";
  sha256 = "1rjzsxp5frzq9iw1mhgcci51n5wiz0dl648ai48rrgdkmgrwgbwk";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    ansi-wl-pprint bzlib Cabal filepath HaXml HUnit mtl network parsec
    process-extras pureMD5 regex-compat regex-tdfa text time Unixutils
    utf8-string zlib
  ];
  doCheck = false;
  meta = {
    homepage = "http://src.seereason.com/haskell-debian";
    description = "Modules for working with the Debian package system";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
