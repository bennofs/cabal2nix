{ cabal, cabal-file-th, cmdargs, Diff, filemanip, filepath, HUnit, parsec
, regex-tdfa, test-framework, test-framework-hunit, utf8-string
}:

cabal.mkDerivation (self: {
  pname = "shelltestrunner";
  version = "1.3.1";
  sha256 = "0458794w4c4c4q049sw6ddzxqghzggjh20s1dgw7k4dr5s2srlb6";
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    cabal-file-th cmdargs Diff filemanip filepath HUnit parsec regex-tdfa
    test-framework test-framework-hunit utf8-string
  ];
  doCheck = false;
  meta = {
    homepage = "http://joyful.com/shelltestrunner";
    description = "A tool for testing command-line programs";
    license = "GPL";
    platforms = self.ghc.meta.platforms;
  };
})
