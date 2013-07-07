{ cabal, binary, event-list, explicit-exception, monoid-transformer
, non-negative, QuickCheck, random, transformers
}:

cabal.mkDerivation (self: {
  pname = "midi";
  version = "0.2.0.1";
  sha256 = "1g82r4addsphvlw1l2lhlrn7wj0jvv4s2wnyl4f54axj3j4h7vn3";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    binary event-list explicit-exception monoid-transformer non-negative
    QuickCheck random transformers
  ];
  doCheck = false;
  meta = {
    homepage = "http://www.haskell.org/haskellwiki/MIDI";
    description = "Handling of MIDI messages and files";
    license = "GPL";
    platforms = self.ghc.meta.platforms;
  };
})
