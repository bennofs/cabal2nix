{ cabal, resource-pool, resourcet, transformers }:

cabal.mkDerivation (self: {
  pname = "pool-conduit";
  version = "0.1.1";
  sha256 = "1gaj0gwwbr950jkxhpl326py758j2mvh557xz737js9qcs9g3cg1";
  buildDepends = [ resource-pool resourcet transformers ];
  doCheck = false;
  meta = {
    homepage = "http://www.yesodweb.com/book/persistent";
    description = "Resource pool allocations via ResourceT";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
