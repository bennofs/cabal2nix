{ cabal, aeson, filepath, snap-core, snap-server, text, time, transformers
, unordered-containers
}:

cabal.mkDerivation (self: {
  pname = "ekg";
  version = "0.3.1.2";
  sha256 = "0m18jw4hc9bg7i48ch5m90gq1gimx852j1hb7hk0g6llyfinazgq";
  buildDepends = [
    aeson filepath snap-core snap-server text time transformers
    unordered-containers
  ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/tibbe/ekg";
    description = "Remote monitoring of processes";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
