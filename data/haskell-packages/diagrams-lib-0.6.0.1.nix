{ cabal, active, colour, data-default, diagrams-core, monoid-extras, newtype
, NumInstances, semigroups, vector-space
}:

cabal.mkDerivation (self: {
  pname = "diagrams-lib";
  version = "0.6.0.1";
  sha256 = "00ysdppl2jv0kspj0pjy8qj8shc9gg6g10lkq62vlvr39wnxx6yj";
  buildDepends = [
    active colour data-default diagrams-core monoid-extras newtype NumInstances
    semigroups vector-space
  ];
  doCheck = false;
  meta = {
    homepage = "http://projects.haskell.org/diagrams";
    description = "Embedded domain-specific language for declarative graphics";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
