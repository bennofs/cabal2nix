{ cabal, data-default, libX11, libXext, libXinerama, libXrandr, libXrender }:

cabal.mkDerivation (self: {
  pname = "X11";
  version = "1.6.1.1";
  sha256 = "1bkfnxcmf8qia0l3x5n3j4f1zakjwnlq0mhdnbpp6v3q2g37brbw";
  buildDepends = [ data-default ];
  extraLibraries = [ libX11 libXext libXinerama libXrandr libXrender ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/haskell-pkg-janitors/X11";
    description = "A binding to the X11 graphics library";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
