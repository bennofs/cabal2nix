{ cabal, cereal, data-default, HUnit, idna, text, utf8-string }:

cabal.mkDerivation (self: {
  pname = "publicsuffixlist";
  version = "0.1";
  sha256 = "0mbrmhgyjp8jms3fd3nq4knc4j97sw8ijrmnlfjs7qj8jw4vwzxk";
  buildDepends = [ cereal data-default text utf8-string ];
  testDepends = [ cereal data-default HUnit idna text utf8-string ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/litherum/publicsuffixlist";
    description = "Is a given string a domain suffix?";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
