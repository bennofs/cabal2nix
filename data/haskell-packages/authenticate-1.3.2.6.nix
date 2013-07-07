{ cabal, aeson, attoparsec, blaze-builder, case-insensitive, conduit
, http-conduit, http-types, network, tagsoup, text, transformers
, unordered-containers, xml-conduit
}:

cabal.mkDerivation (self: {
  pname = "authenticate";
  version = "1.3.2.6";
  sha256 = "12sgi6q6kajjhh8mns9nklxj0kwn32xs5kzi7wmw50shx0smnjrz";
  buildDepends = [
    aeson attoparsec blaze-builder case-insensitive conduit http-conduit
    http-types network tagsoup text transformers unordered-containers
    xml-conduit
  ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/yesodweb/authenticate";
    description = "Authentication methods for Haskell web applications";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
