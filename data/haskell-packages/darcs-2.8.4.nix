{ cabal, curl, extensible-exceptions, filepath, hashed-storage, haskeline, html
, HTTP, mmap, mtl, network, parsec, random, regex-compat, tar, terminfo, text
, utf8-string, vector, zlib
}:

cabal.mkDerivation (self: {
  pname = "darcs";
  version = "2.8.4";
  sha256 = "164zclgib9ql4rqykpdhhk2bad0m5v0k0iwzsj0z7nax5nxlvarz";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    extensible-exceptions filepath hashed-storage haskeline html HTTP mmap mtl
    network parsec random regex-compat tar terminfo text utf8-string vector
    zlib
  ];
  extraLibraries = [ curl ];
  doCheck = false;
  postInstall = ''
    mkdir -p $out/etc/bash_completion.d
    mv contrib/darcs_completion $out/etc/bash_completion.d/darcs
  '';
  meta = {
    homepage = "http://darcs.net/";
    description = "a distributed, interactive, smart revision control system";
    license = "GPL";
    platforms = self.ghc.meta.platforms;
  };
})
