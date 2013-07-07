{ cabal, hashable, monad-control, stm, time, transformers, transformers-base
, vector
}:

cabal.mkDerivation (self: {
  pname = "resource-pool";
  version = "0.2.1.1";
  sha256 = "1ypyzy7mkmpab6rghsizrx6raam3l2acwxm56x7jmcv8s2algi1g";
  buildDepends = [
    hashable monad-control stm time transformers transformers-base vector
  ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/bos/pool";
    description = "A high-performance striped resource pooling implementation";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
