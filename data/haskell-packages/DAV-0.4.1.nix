{ cabal, case-insensitive, http-conduit, http-types, lens, lifted-base, mtl
, network, optparse-applicative, resourcet, transformers, xml-conduit
, xml-hamlet
}:

cabal.mkDerivation (self: {
  pname = "DAV";
  version = "0.4.1";
  sha256 = "0bcrnlixrzvbdvw7ffv2xl2d0k0w71jf0i5ayf97ymxly8ii8s0c";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    case-insensitive http-conduit http-types lens lifted-base mtl network
    optparse-applicative resourcet transformers xml-conduit xml-hamlet
  ];
  doCheck = false;
  meta = {
    homepage = "http://floss.scru.org/hDAV";
    description = "RFC 4918 WebDAV support";
    license = self.stdenv.lib.licenses.gpl3;
    platforms = self.ghc.meta.platforms;
  };
})
