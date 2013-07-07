{ cabal, chell, HUnit }:

cabal.mkDerivation (self: {
  pname = "chell-hunit";
  version = "0.2";
  sha256 = "035c1xgnqbxq810dn5hxlvwfwb4p6qc13p0vg8ylx0vn073jgvhr";
  buildDepends = [ chell HUnit ];
  doCheck = false;
  meta = {
    homepage = "https://john-millikin.com/software/chell/";
    description = "HUnit support for the Chell testing library";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
