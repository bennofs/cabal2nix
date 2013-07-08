{ cabal, bzip2, filepath, freetype2, GraphicsMagick, jasper, lcms, libjpeg
, libpng, libxml2, tiff, wmflite, zlib
}:

cabal.mkDerivation (self: {
  pname = "hsmagick";
  version = "0.5";
  sha256 = "1bfzbwddss0m0z4jf7i0b06pmxy9rvknpqnzhf0v5jggv5nr442p";
  buildDepends = [ filepath ];
  extraLibraries = [ bzip2 jasper libjpeg libpng tiff wmflite zlib ];
  pkgconfigDepends = [ freetype2 GraphicsMagick lcms libxml2 ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/vincentg/hsmagick";
    description = "FFI bindings for the GraphicsMagick library";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
