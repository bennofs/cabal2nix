{ cabal, ansi-terminal, options, patience, random, system-filepath, text
, transformers
}:

cabal.mkDerivation (self: {
  pname = "chell";
  version = "0.3";
  sha256 = "11kp9s0c6k0p7gw6y2hqwdai7pj0s83b4k11qnv4pqkmi9d8piai";
  buildDepends = [
    ansi-terminal options patience random system-filepath text transformers
  ];
  doCheck = false;
  meta = {
    homepage = "https://john-millikin.com/software/chell/";
    description = "A simple and intuitive library for automated testing";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
