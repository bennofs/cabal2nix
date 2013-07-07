{ cabal }:

cabal.mkDerivation (self: {
  pname = "hsql";
  version = "1.8.2";
  sha256 = "0i53n42ynq22fzlz4kpmri4q4abmi4dz8bz0izn307is1pmk4bby";
  doCheck = false;
  meta = {
    description = "Database access from Haskell";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
