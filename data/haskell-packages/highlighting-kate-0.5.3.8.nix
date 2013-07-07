{ cabal, blaze-html, filepath, mtl, parsec, regex-pcre }:

cabal.mkDerivation (self: {
  pname = "highlighting-kate";
  version = "0.5.3.8";
  sha256 = "1fdgmwhiw05mgbgsrgyzvdqzklw9r10nswxfpnxkh5ivfxrhz82w";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [ blaze-html filepath mtl parsec regex-pcre ];
  doCheck = false;
  prePatch = "sed -i -e 's|regex-pcre-builtin|regex-pcre|' highlighting-kate.cabal";
  meta = {
    homepage = "http://github.com/jgm/highlighting-kate";
    description = "Syntax highlighting";
    license = "GPL";
    platforms = self.ghc.meta.platforms;
  };
})
