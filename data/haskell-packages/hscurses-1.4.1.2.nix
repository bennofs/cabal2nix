{ cabal, mtl }:

cabal.mkDerivation (self: {
  pname = "hscurses";
  version = "1.4.1.2";
  sha256 = "1r3njcwad8d4vn17s5g69iw3rhjz9hbhv49wkqqpndck5080fh7l";
  buildDepends = [ mtl ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/skogsbaer/hscurses";
    description = "NCurses bindings for Haskell";
    license = "LGPL";
    platforms = self.ghc.meta.platforms;
  };
})
