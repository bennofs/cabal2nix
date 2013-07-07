{ cabal, happstack-server, mtl, random, reform, utf8-string }:

cabal.mkDerivation (self: {
  pname = "reform-happstack";
  version = "0.1.2";
  sha256 = "02rq4pq0a6157z59zqfww428sfv03cv2czvd6d4jk3q7s48dvpfq";
  buildDepends = [ happstack-server mtl random reform utf8-string ];
  doCheck = false;
  meta = {
    homepage = "http://www.happstack.com/";
    description = "Happstack support for reform";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
