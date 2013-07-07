{ cabal, network, time }:

cabal.mkDerivation (self: {
  pname = "simpleirc";
  version = "0.2.1";
  sha256 = "0zvv0ym9vvzg3xhx4yc9l8l7yd312r047zvzlib056lzgmsayiv8";
  buildDepends = [ network time ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/dom96/SimpleIRC";
    description = "Simple IRC Library";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
