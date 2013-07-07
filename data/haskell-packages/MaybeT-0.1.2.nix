{ cabal, mtl }:

cabal.mkDerivation (self: {
  pname = "MaybeT";
  version = "0.1.2";
  sha256 = "0cmnfs22ldai0z172rdsvryzsh33a70yax21v03nhr92a4b62plr";
  buildDepends = [ mtl ];
  doCheck = false;
  meta = {
    description = "MaybeT monad transformer";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
