{ cabal, comonad-transformers, comonads-fd, free, semigroupoids, semigroups
, transformers
}:

cabal.mkDerivation (self: {
  pname = "keys";
  version = "3.0.2";
  sha256 = "0ln8mwyahpx99lh4889rdpk6jd30ja6d5mvm4c9vcx76pr579nys";
  buildDepends = [
    comonad-transformers comonads-fd free semigroupoids semigroups transformers
  ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/ekmett/keys/";
    description = "Keyed functors and containers";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
