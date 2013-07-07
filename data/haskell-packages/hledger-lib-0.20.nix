{ cabal, cmdargs, csv, filepath, HUnit, mtl, parsec, pretty-show, regex-compat
, regexpr, safe, split, time, transformers, utf8-string
}:

cabal.mkDerivation (self: {
  pname = "hledger-lib";
  version = "0.20";
  sha256 = "040r797whajgy5xvb8ixlj5w8izx2q42mgd5554pzvjys18lsb5j";
  buildDepends = [
    cmdargs csv filepath HUnit mtl parsec pretty-show regex-compat regexpr safe
    split time transformers utf8-string
  ];
  doCheck = false;
  meta = {
    homepage = "http://hledger.org";
    description = "Core data types, parsers and utilities for the hledger accounting tool";
    license = "GPL";
    platforms = self.ghc.meta.platforms;
  };
})
