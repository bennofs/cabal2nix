{ cabal, data-default, hamlet, network-conduit, safe, shakespeare-css
, shakespeare-js, text, transformers, unordered-containers, wai, wai-extra
, warp, yaml, yesod-core
}:

cabal.mkDerivation (self: {
  pname = "yesod-default";
  version = "1.1.3.2";
  sha256 = "07gm9sj4c231wdmfmb7df1s8wvqa6fw7nhcq554h16h2ibv5pcqg";
  buildDepends = [
    data-default hamlet network-conduit safe shakespeare-css shakespeare-js
    text transformers unordered-containers wai wai-extra warp yaml yesod-core
  ];
  doCheck = false;
  meta = {
    homepage = "http://www.yesodweb.com/";
    description = "Default config and main functions for your yesod application";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
