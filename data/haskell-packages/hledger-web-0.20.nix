{ cabal, blaze-html, blaze-markup, clientsession, cmdargs, data-default
, filepath, hamlet, hjsmin, hledger, hledger-lib, http-conduit, HUnit
, monad-control, network-conduit, parsec, regexpr, safe, shakespeare-css
, shakespeare-js, shakespeare-text, text, time, transformers, wai, wai-extra
, wai-handler-launch, warp, yaml, yesod, yesod-core, yesod-default, yesod-form
, yesod-static, yesod-test
}:

cabal.mkDerivation (self: {
  pname = "hledger-web";
  version = "0.20";
  sha256 = "0imjr2cl68f6xp1vqv35z6pvbdmlbr9knsydwiazmw5s13r8sj21";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    blaze-html blaze-markup clientsession cmdargs data-default filepath hamlet
    hjsmin hledger hledger-lib http-conduit HUnit monad-control network-conduit
    parsec regexpr safe shakespeare-css shakespeare-js shakespeare-text text
    time transformers wai wai-extra wai-handler-launch warp yaml yesod
    yesod-core yesod-default yesod-form yesod-static
  ];
  testDepends = [ yesod-core yesod-default yesod-test ];
  doCheck = false;
  meta = {
    homepage = "http://hledger.org";
    description = "A web interface for the hledger accounting tool";
    license = "GPL";
    platforms = self.ghc.meta.platforms;
  };
})
