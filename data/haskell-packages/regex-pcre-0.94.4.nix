{ cabal, pcre, regex-base }:

cabal.mkDerivation (self: {
  pname = "regex-pcre";
  version = "0.94.4";
  sha256 = "1h16w994g9s62iwkdqa7bar2n9cfixmkzz2rm8svm960qr57valf";
  buildDepends = [ regex-base ];
  extraLibraries = [ pcre ];
  doCheck = false;
  meta = {
    homepage = "http://hackage.haskell.org/package/regex-pcre";
    description = "Replaces/Enhances Text.Regex";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
