{ cabal, aeson, deepseq, filepath, hastache, mtl, mwc-random, parsec
, statistics, time, transformers, vector, vector-algorithms
}:

cabal.mkDerivation (self: {
  pname = "criterion";
  version = "0.6.2.0";
  sha256 = "1xd90qb026niq2sn7ks8bn92ifb6255saic68bzg6kzj7ydwwdmx";
  buildDepends = [
    aeson deepseq filepath hastache mtl mwc-random parsec statistics time
    transformers vector vector-algorithms
  ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/bos/criterion";
    description = "Robust, reliable performance measurement and analysis";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
