{ cabal, filepath, irc, mtl, network, parsec, random, stm, time }:

cabal.mkDerivation (self: {
  pname = "ircbot";
  version = "0.5.3";
  sha256 = "04fn373va8z8lidpligwficblhsrd6mfzyrw8y20l9544npzxcks";
  buildDepends = [ filepath irc mtl network parsec random stm time ];
  doCheck = false;
  meta = {
    homepage = "http://hub.darcs.net/stepcut/ircbot";
    description = "A library for writing irc bots";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
