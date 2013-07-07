{ cabal, dual-tree, MemoTrie, monoid-extras, newtype, semigroups, vector-space
, vector-space-points
}:

cabal.mkDerivation (self: {
  pname = "diagrams-core";
  version = "0.6.0.1";
  sha256 = "0kw0rxk9a2zkpnbx4bfd0japm75y29ldvdn7i3c93kvz0p6jc2wa";
  buildDepends = [
    dual-tree MemoTrie monoid-extras newtype semigroups vector-space
    vector-space-points
  ];
  doCheck = false;
  meta = {
    homepage = "http://projects.haskell.org/diagrams";
    description = "Core libraries for diagrams EDSL";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
