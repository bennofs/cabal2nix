{ cabal, cmdargs, filepath, haskeline, hledger-lib, HUnit, mtl, parsec, regexpr
, safe, shakespeare-text, split, text, time, utf8-string
}:

cabal.mkDerivation (self: {
  pname = "hledger";
  version = "0.20";
  sha256 = "1jdh01y8jys1ha3qrmx509ka4wb1bgv28xz3rwz8aklz2nfzn4zb";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    cmdargs filepath haskeline hledger-lib HUnit mtl parsec regexpr safe
    shakespeare-text split text time utf8-string
  ];
  doCheck = false;
  meta = {
    homepage = "http://hledger.org";
    description = "The main command-line interface for the hledger accounting tool";
    license = "GPL";
    platforms = self.ghc.meta.platforms;
  };
})
