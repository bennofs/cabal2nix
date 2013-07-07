{ cabal, hslogger, MissingH, mtl, network, parsec, regex-compat }:

cabal.mkDerivation (self: {
  pname = "ftphs";
  version = "1.0.8";
  sha256 = "1jin53srkv265fig9vas6mwg32smpxzmbjh98v0ginb0ygx117lg";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [ hslogger MissingH mtl network parsec regex-compat ];
  doCheck = false;
  meta = {
    homepage = "http://software.complete.org/ftphs";
    description = "FTP Client and Server Library";
    license = "LGPL";
    platforms = self.ghc.meta.platforms;
  };
})
