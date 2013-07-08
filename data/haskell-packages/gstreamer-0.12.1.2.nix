{ cabal, glib, gstreamer, gstreamer-audio, gstreamer-base, gstreamer-controller
, gstreamer-dataprotocol, gstreamer-net, gstreamer-plugins-base
, gtk2hs-buildtools, mtl
}:

cabal.mkDerivation (self: {
  pname = "gstreamer";
  version = "0.12.1.2";
  sha256 = "1kb3l6fgvydfjcw9fni23al6l8l1cpmcnwv17jr4070l85l4f5xd";
  buildDepends = [ glib mtl ];
  buildTools = [ gtk2hs-buildtools ];
  pkgconfigDepends = [
    gstreamer gstreamer-audio gstreamer-base gstreamer-controller
    gstreamer-dataprotocol gstreamer-net gstreamer-plugins-base
  ];
  doCheck = false;
  meta = {
    homepage = "http://projects.haskell.org/gtk2hs/";
    description = "Binding to the GStreamer open source multimedia framework";
    license = self.stdenv.lib.licenses.lgpl21;
    platforms = self.ghc.meta.platforms;
  };
})
