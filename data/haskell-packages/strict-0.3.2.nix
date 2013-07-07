{ cabal }:

cabal.mkDerivation (self: {
  pname = "strict";
  version = "0.3.2";
  sha256 = "08cjajqz9h47fkq98mlf3rc8n5ghbmnmgn8pfsl3bdldjdkmmlrc";
  doCheck = false;
  meta = {
    homepage = "http://www.cse.unsw.edu.au/~rl/code/strict.html";
    description = "Strict data types and String IO";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
