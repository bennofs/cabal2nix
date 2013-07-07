{ cabal, mtl }:

cabal.mkDerivation (self: {
  pname = "mtlparse";
  version = "0.1.2";
  sha256 = "0hnixpchgzrlx83cqykmqnahk8llf18gz8jkq2ql5y7gwj2xk1fd";
  buildDepends = [ mtl ];
  doCheck = false;
  meta = {
    homepage = "http://homepage3.nifty.com/salamander/second/projects/mtlparse/";
    description = "parse library using mtl package";
    license = "LGPL";
    platforms = self.ghc.meta.platforms;
  };
})
