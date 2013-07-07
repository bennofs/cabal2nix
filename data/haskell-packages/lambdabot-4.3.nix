{ cabal, arrows, binary, brainfuck, data-memocombinators, dependent-map
, dependent-sum, dependent-sum-template, dice, edit-distance, filepath
, haskeline, haskell-src-exts, hoogle, hslogger, hstatsd, HTTP, IOSpec
, lifted-base, logict, misfortune, monad-control, MonadRandom, mtl, mueval
, network, numbers, oeis, parsec, QuickCheck, random, random-fu, random-source
, regex-tdfa, SafeSemaphore, show, split, syb, tagsoup, time, transformers
, transformers-base, unlambda, utf8-string, vector-space, zlib
}:

cabal.mkDerivation (self: {
  pname = "lambdabot";
  version = "4.3";
  sha256 = "0pjwxlq4rbmg9wj44vrillly967y35b4i995mz5167hpji05clvy";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    arrows binary brainfuck data-memocombinators dependent-map dependent-sum
    dependent-sum-template dice edit-distance filepath haskeline
    haskell-src-exts hoogle hslogger hstatsd HTTP IOSpec lifted-base logict
    misfortune monad-control MonadRandom mtl mueval network numbers oeis parsec
    QuickCheck random random-fu random-source regex-tdfa SafeSemaphore show
    split syb tagsoup time transformers transformers-base unlambda utf8-string
    vector-space zlib
  ];
  doCheck = false;
  meta = {
    homepage = "http://haskell.org/haskellwiki/Lambdabot";
    description = "Lambdabot is a development tool and advanced IRC bot";
    license = "GPL";
    platforms = self.ghc.meta.platforms;
  };
})
