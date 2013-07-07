{ cabal, mtl, stm, text }:

cabal.mkDerivation (self: {
  pname = "web-plugins";
  version = "0.2.1";
  sha256 = "1gly0v2xldgfd73azldwxnhhxqj5pmv0f6jn8cm4sjarsj1jcc0c";
  buildDepends = [ mtl stm text ];
  doCheck = false;
  meta = {
    homepage = "http://www.happstack.com/";
    description = "dynamic plugin system for web applications";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
