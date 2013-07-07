{ nixpkgs ? import <nixpkgs> {}, cabal ? nixpkgs.haskellPackages.cabal }:

let

  base-compat = null;
  binary = null;
  Cabal = null;
  checkers = null;
  concurrent-extra = null;
  deepseq = null;
  diagrams-contrib = null;
  dice = null;
  double-conversion = null;
  encoding = null;
  filepath = null;
  Glob = null;
  hexpat = null;
  hspec-meta = null;
  hstatsd = null;
  httpd-shed = null;
  libdpkg = null;
  markdown-unlit = null;
  misfortune = null;
  nanospec = null;
  process-leksah = null;
  random-fu = null;
  random-source = null;
  stringbuilder = null;
  test-framework-doctest = null;
  time = null;
  transformers-compat = null;
  uniqueid = null;
  utf8String = null;

in

rec {

  acid-state = import ./acid-state-0.8.1.nix {
    inherit cabal cereal extensible-exceptions filepath mtl network safecopy stm;
  };

  active = import ./active-0.1.0.4.nix {
    inherit cabal newtype QuickCheck semigroupoids semigroups vector-space;
  };

  adjunctions = import ./adjunctions-3.0.1.nix {
    inherit cabal comonad comonad-transformers contravariant distributive keys mtl representable-functors semigroupoids transformers void;
  };

  aeson = import ./aeson-0.6.1.0.nix {
    inherit cabal attoparsec blaze-builder deepseq dlist hashable mtl QuickCheck syb test-framework test-framework-quickcheck2 text time unordered-containers vector;
  };

  Agda = import ./Agda-2.3.2.1.nix {
    inherit cabal binary deepseq filepath geniplate hashable hashtables haskeline haskell-src-exts mtl parallel QuickCheck text time unordered-containers xhtml zlib;
    inherit (nixpkgs) alex happy;
  };

  alex = import ./alex-3.0.5.nix {
    inherit cabal QuickCheck;
    inherit (nixpkgs) perl;
  };

  algebra = import ./algebra-3.1.nix {
    inherit cabal categories distributive keys mtl nats representable-functors representable-tries semigroupoids semigroups tagged transformers void;
  };

  ALUT = import ./ALUT-2.2.0.0.nix {
    inherit cabal OpenAL StateVar;
  };

  AMI = import ./AMI-0.1.nix {
    inherit cabal mtl network pureMD5 stm;
  };

  ansi-terminal = import ./ansi-terminal-0.5.5.nix {
    inherit cabal;
  };

  ansi-wl-pprint = import ./ansi-wl-pprint-0.6.4.nix {
    inherit cabal ansi-terminal;
  };

  arrows = import ./arrows-0.4.4.1.nix {
    inherit cabal Stream;
  };

  asn1-data = import ./asn1-data-0.7.1.nix {
    inherit cabal cereal mtl text;
  };

  asn1-types = import ./asn1-types-0.2.0.nix {
    inherit cabal time;
  };

  async = import ./async-2.0.1.4.nix {
    inherit cabal HUnit stm test-framework test-framework-hunit;
  };

  attempt = import ./attempt-0.4.0.nix {
    inherit cabal failure;
  };

  attoparsec = import ./attoparsec-0.10.4.0.nix {
    inherit cabal deepseq QuickCheck test-framework test-framework-quickcheck2 text;
  };

  attoparsec-conduit = import ./attoparsec-conduit-1.0.0.nix {
    inherit cabal attoparsec conduit hspec resourcet text transformers;
  };

  attoparsec-enumerator = import ./attoparsec-enumerator-0.3.1.nix {
    inherit cabal attoparsec enumerator text;
  };

  augeas = import ./augeas-0.6.1.nix {
    inherit cabal HUnit;
    inherit (nixpkgs) augeas;
  };

  authenticate = import ./authenticate-1.3.2.6.nix {
    inherit cabal aeson attoparsec blaze-builder case-insensitive conduit http-conduit http-types network tagsoup text transformers unordered-containers xml-conduit;
  };

  authenticate-oauth = import ./authenticate-oauth-1.4.0.6.nix {
    inherit cabal base64-bytestring blaze-builder blaze-builder-conduit conduit crypto-pubkey-types data-default http-conduit http-types monad-control random resourcet RSA SHA time transformers;
  };

  base16-bytestring = import ./base16-bytestring-0.1.1.5.nix {
    inherit cabal;
  };

  base64-bytestring = import ./base64-bytestring-1.0.0.0.nix {
    inherit cabal HUnit QuickCheck test-framework test-framework-hunit test-framework-quickcheck2;
  };

  base64-conduit = import ./base64-conduit-1.0.0.nix {
    inherit cabal base64-bytestring conduit hspec QuickCheck transformers;
  };

  base-unicode-symbols = import ./base-unicode-symbols-0.2.2.4.nix {
    inherit cabal;
  };

  basic-prelude = import ./basic-prelude-0.3.4.0.nix {
    inherit cabal hashable lifted-base ReadArgs system-filepath text transformers unordered-containers vector;
  };

  bifunctors = import ./bifunctors-3.2.0.1.nix {
    inherit cabal semigroupoids semigroups tagged;
  };

  binary-communicator = import ./binary-communicator-1.0.2.1.nix {
    inherit cabal binary mtl;
  };

  binary-shared = import ./binary-shared-0.8.2.nix {
    inherit cabal binary mtl;
  };

  bindings-DSL = import ./bindings-DSL-1.0.16.nix {
    inherit cabal;
  };

  bindings-gpgme = import ./bindings-gpgme-0.1.4.nix {
    inherit cabal bindings-DSL;
    inherit (nixpkgs) gpgme;
  };

  bindings-libzip = import ./bindings-libzip-0.10.2.nix {
    inherit cabal bindings-DSL;
    inherit (nixpkgs) libzip;
  };

  bindings-sane = import ./bindings-sane-0.0.1.nix {
    inherit cabal bindings-DSL;
    inherit (nixpkgs) sane-backends;
  };

  bitarray = import ./bitarray-0.0.1.nix {
    inherit cabal;
  };

  blaze-builder = import ./blaze-builder-0.3.1.0.nix {
    inherit cabal text;
  };

  blaze-builder-conduit = import ./blaze-builder-conduit-1.0.0.nix {
    inherit cabal blaze-builder conduit hspec QuickCheck text transformers;
  };

  blaze-builder-enumerator = import ./blaze-builder-enumerator-0.2.0.5.nix {
    inherit cabal blaze-builder enumerator transformers;
  };

  blaze-html = import ./blaze-html-0.6.1.1.nix {
    inherit cabal blaze-builder blaze-markup HUnit QuickCheck test-framework test-framework-hunit test-framework-quickcheck2 text;
  };

  blaze-markup = import ./blaze-markup-0.5.1.5.nix {
    inherit cabal blaze-builder HUnit QuickCheck test-framework test-framework-hunit test-framework-quickcheck2 text;
  };

  blaze-svg = import ./blaze-svg-0.3.3.0.nix {
    inherit cabal blaze-markup mtl;
  };

  blaze-textual = import ./blaze-textual-0.2.0.8.nix {
    inherit cabal blaze-builder double-conversion QuickCheck test-framework test-framework-quickcheck2 text time vector;
  };

  bloomfilter = import ./bloomfilter-1.2.6.10.nix {
    inherit cabal deepseq;
  };

  bluetile = import ./bluetile-0.6.nix {
    inherit cabal ConfigFile filepath glade gtk mtl random regex-compat utf8-string X11 X11-xft xmonad xmonad-contrib;
  };

  bmp = import ./bmp-1.2.3.4.nix {
    inherit cabal binary;
  };

  Boolean = import ./Boolean-0.1.2.nix {
    inherit cabal;
  };

  boomerang = import ./boomerang-1.3.1.nix {
    inherit cabal mtl;
  };

  BoundedChan = import ./BoundedChan-1.0.1.0.nix {
    inherit cabal;
  };

  brainfuck = import ./brainfuck-0.1.nix {
    inherit cabal mtl;
  };

  byteorder = import ./byteorder-1.0.3.nix {
    inherit cabal;
  };

  bytestring-lexing = import ./bytestring-lexing-0.4.0.nix {
    inherit cabal;
    inherit (nixpkgs) alex;
  };

  bytestring-mmap = import ./bytestring-mmap-0.2.2.nix {
    inherit cabal;
  };

  bytestring-nums = import ./bytestring-nums-0.3.5.nix {
    inherit cabal;
  };

  bytestring-progress = import ./bytestring-progress-1.0.3.nix {
    inherit cabal terminal-progress-bar time;
  };

  bytestring-show = import ./bytestring-show-0.3.5.4.nix {
    inherit cabal binary;
  };

  bzlib = import ./bzlib-0.5.0.4.nix {
    inherit cabal;
    inherit (nixpkgs) bz2;
  };

  c2hs = import ./c2hs-0.16.4.nix {
    inherit cabal filepath language-c;
  };

  cabal-debian = import ./cabal-debian-1.25.nix {
    inherit cabal Cabal debian filepath mtl parsec regex-tdfa Unixutils utf8-string;
  };

  cabal-file-th = import ./cabal-file-th-0.2.3.nix {
    inherit cabal Cabal;
  };

  cabal-install = import ./cabal-install-1.16.0.2.nix {
    inherit cabal Cabal filepath HTTP mtl network random time zlib;
  };

  cairo = import ./cairo-0.12.4.nix {
    inherit cabal mtl;
    inherit (nixpkgs) cairo gtk2hsBuildtools libc pkgconfig zlib;
  };

  carettah = import ./carettah-0.1.2.nix {
    inherit cabal cairo filepath gtk hcwiid highlighting-kate mtl pandoc pango time;
  };

  case-insensitive = import ./case-insensitive-1.0.0.1.nix {
    inherit cabal deepseq hashable HUnit test-framework test-framework-hunit text;
  };

  categories = import ./categories-1.0.4.nix {
    inherit cabal void;
  };

  cautious-file = import ./cautious-file-1.0.2.nix {
    inherit cabal filepath;
  };

  cereal = import ./cereal-0.3.5.2.nix {
    inherit cabal;
  };

  cereal-conduit = import ./cereal-conduit-0.7.nix {
    inherit cabal cereal conduit HUnit mtl resourcet transformers;
  };

  certificate = import ./certificate-1.3.8.nix {
    inherit cabal asn1-data crypto-pubkey-types cryptohash filepath mtl pem time;
  };

  cgi = import ./cgi-3001.1.8.3.nix {
    inherit cabal extensible-exceptions MonadCatchIO-mtl mtl network parsec xhtml;
  };

  Chart = import ./Chart-0.16.nix {
    inherit cabal cairo colour data-accessor data-accessor-template mtl time;
  };

  ChasingBottoms = import ./ChasingBottoms-1.3.0.6.nix {
    inherit cabal mtl QuickCheck random syb;
  };

  chell = import ./chell-0.3.nix {
    inherit cabal ansi-terminal options patience random system-filepath text transformers;
  };

  chell-hunit = import ./chell-hunit-0.2.nix {
    inherit cabal chell HUnit;
  };

  chell-quickcheck = import ./chell-quickcheck-0.2.2.nix {
    inherit cabal chell QuickCheck random;
  };

  cipher-aes = import ./cipher-aes-0.1.8.nix {
    inherit cabal QuickCheck test-framework test-framework-quickcheck2;
  };

  cipher-rc4 = import ./cipher-rc4-0.1.2.nix {
    inherit cabal QuickCheck test-framework test-framework-quickcheck2;
  };

  citeproc-hs = import ./citeproc-hs-0.3.8.nix {
    inherit cabal filepath hexpat hs-bibutils HTTP json mtl network pandoc-types parsec syb time utf8-string;
  };

  classy-prelude = import ./classy-prelude-0.5.4.nix {
    inherit cabal basic-prelude hashable hspec lifted-base QuickCheck system-filepath text transformers unordered-containers vector;
  };

  classy-prelude-conduit = import ./classy-prelude-conduit-0.5.3.nix {
    inherit cabal classy-prelude conduit hspec monad-control QuickCheck resourcet transformers void xml-conduit;
  };

  clientsession = import ./clientsession-0.9.nix {
    inherit cabal base64-bytestring cereal cipher-aes cprng-aes crypto-api entropy hspec HUnit QuickCheck skein tagged transformers;
  };

  clock = import ./clock-0.2.0.0.nix {
    inherit cabal;
  };

  clocked = import ./clocked-0.4.1.2.nix {
    inherit cabal clock MonadCatchIO-transformers transformers;
    inherit (nixpkgs) QtCore;
  };

  cmdargs = import ./cmdargs-0.10.1.nix {
    inherit cabal filepath transformers;
  };

  colour = import ./colour-2.3.3.nix {
    inherit cabal;
  };

  comonad = import ./comonad-3.0.1.1.nix {
    inherit cabal semigroups transformers;
  };

  comonads-fd = import ./comonads-fd-3.0.1.nix {
    inherit cabal comonad comonad-transformers mtl semigroups transformers;
  };

  comonad-transformers = import ./comonad-transformers-3.0.1.nix {
    inherit cabal comonad contravariant distributive semigroupoids semigroups transformers;
  };

  concrete-typerep = import ./concrete-typerep-0.1.0.2.nix {
    inherit cabal binary hashable QuickCheck test-framework test-framework-quickcheck2;
  };

  cond = import ./cond-0.4.0.2.nix {
    inherit cabal;
  };

  conduit = import ./conduit-1.0.5.1.nix {
    inherit cabal doctest hspec lifted-base mmorph monad-control QuickCheck resourcet text transformers transformers-base void;
  };

  ConfigFile = import ./ConfigFile-1.1.1.nix {
    inherit cabal MissingH mtl parsec;
  };

  configurator = import ./configurator-0.2.0.1.nix {
    inherit cabal attoparsec hashable text unix-compat unordered-containers;
  };

  contravariant = import ./contravariant-0.4.1.nix {
    inherit cabal tagged transformers transformers-compat;
  };

  control-monad-loop = import ./control-monad-loop-0.1.nix {
    inherit cabal transformers transformers-base;
  };

  convertible = import ./convertible-1.0.11.1.nix {
    inherit cabal mtl text time;
  };

  cookie = import ./cookie-0.4.0.1.nix {
    inherit cabal blaze-builder data-default text time;
  };

  cpphs = import ./cpphs-1.16.nix {
    inherit cabal;
  };

  cprng-aes = import ./cprng-aes-0.3.4.nix {
    inherit cabal cipher-aes crypto-api crypto-random-api entropy random;
  };

  cpu = import ./cpu-0.1.1.nix {
    inherit cabal;
  };

  criterion = import ./criterion-0.6.2.0.nix {
    inherit cabal aeson deepseq filepath hastache mtl mwc-random parsec statistics time transformers vector vector-algorithms;
  };

  Crypto = import ./Crypto-4.2.5.nix {
    inherit cabal HUnit QuickCheck random;
  };

  crypto-api = import ./crypto-api-0.12.2.1.nix {
    inherit cabal cereal entropy tagged transformers;
  };

  cryptocipher = import ./cryptocipher-0.5.0.nix {
    inherit cabal cereal cipher-aes cipher-rc4 cpu crypto-api cryptohash entropy primitive QuickCheck test-framework test-framework-quickcheck2 vector;
  };

  crypto-conduit = import ./crypto-conduit-0.5.2.nix {
    inherit cabal cereal conduit crypto-api cryptocipher cryptohash-cryptoapi hspec skein transformers;
  };

  cryptohash = import ./cryptohash-0.8.4.nix {
    inherit cabal cereal crypto-api HUnit QuickCheck tagged test-framework test-framework-hunit test-framework-quickcheck2;
  };

  cryptohash-cryptoapi = import ./cryptohash-cryptoapi-0.1.0.nix {
    inherit cabal cereal crypto-api cryptohash tagged;
  };

  crypto-numbers = import ./crypto-numbers-0.1.3.nix {
    inherit cabal crypto-random-api HUnit QuickCheck test-framework test-framework-hunit test-framework-quickcheck2 vector;
  };

  crypto-pubkey = import ./crypto-pubkey-0.1.4.nix {
    inherit cabal crypto-numbers crypto-pubkey-types crypto-random-api cryptohash HUnit QuickCheck test-framework test-framework-hunit test-framework-quickcheck2;
  };

  crypto-pubkey-types = import ./crypto-pubkey-types-0.4.0.nix {
    inherit cabal asn1-types;
  };

  crypto-random-api = import ./crypto-random-api-0.2.0.nix {
    inherit cabal entropy;
  };

  css-text = import ./css-text-0.1.1.nix {
    inherit cabal attoparsec hspec HUnit QuickCheck text;
  };

  csv = import ./csv-0.1.2.nix {
    inherit cabal filepath parsec;
  };

  csv-conduit = import ./csv-conduit-0.5.1.nix {
    inherit cabal attoparsec attoparsec-conduit conduit data-default HUnit monad-control test-framework test-framework-hunit text;
  };

  curl = import ./curl-1.3.8.nix {
    inherit cabal;
    inherit (nixpkgs) curl;
  };

  darcs = import ./darcs-2.8.4.nix {
    inherit cabal extensible-exceptions filepath hashed-storage haskeline html HTTP mmap mtl network parsec random regex-compat tar terminfo text utf8-string vector zlib;
    inherit (nixpkgs) curl;
  };

  darcs-monitor = import ./darcs-monitor-0.4.2.nix {
    inherit cabal HaXml mtl;
  };

  data-accessor = import ./data-accessor-0.2.2.3.nix {
    inherit cabal transformers;
  };

  data-accessor-mtl = import ./data-accessor-mtl-0.2.0.3.nix {
    inherit cabal data-accessor mtl;
  };

  data-accessor-template = import ./data-accessor-template-0.2.1.10.nix {
    inherit cabal data-accessor utility-ht;
  };

  data-binary-ieee754 = import ./data-binary-ieee754-0.4.2.1.nix {
    inherit cabal binary;
  };

  data-default = import ./data-default-0.4.0.nix {
    inherit cabal dlist;
  };

  dataenc = import ./dataenc-0.14.0.4.nix {
    inherit cabal;
  };

  data-inttrie = import ./data-inttrie-0.0.8.nix {
    inherit cabal;
  };

  data-lens = import ./data-lens-2.10.2.nix {
    inherit cabal comonad comonad-transformers semigroupoids transformers;
  };

  data-memocombinators = import ./data-memocombinators-0.4.4.nix {
    inherit cabal data-inttrie;
  };

  date-cache = import ./date-cache-0.3.0.nix {
    inherit cabal;
  };

  datetime = import ./datetime-0.2.1.nix {
    inherit cabal QuickCheck time;
  };

  DAV = import ./DAV-0.4.1.nix {
    inherit cabal case-insensitive http-conduit http-types lens lifted-base mtl network optparse-applicative resourcet transformers xml-conduit xml-hamlet;
  };

  dbus = import ./dbus-0.10.4.nix {
    inherit cabal cereal libxml-sax network parsec random text transformers vector xml-types;
  };

  debian = import ./debian-3.70.1.nix {
    inherit cabal ansi-wl-pprint bzlib Cabal filepath HaXml HUnit mtl network parsec process-extras pureMD5 regex-compat regex-tdfa text time Unixutils utf8-string zlib;
  };

  dependent-map = import ./dependent-map-0.1.1.1.nix {
    inherit cabal dependent-sum;
  };

  dependent-sum = import ./dependent-sum-0.2.0.1.nix {
    inherit cabal;
  };

  dependent-sum-template = import ./dependent-sum-template-0.0.0.1.nix {
    inherit cabal dependent-sum th-extras;
  };

  derive = import ./derive-2.5.11.nix {
    inherit cabal filepath haskell-src-exts syb transformers uniplate;
  };

  diagrams = import ./diagrams-0.6.nix {
    inherit cabal diagrams-contrib diagrams-core diagrams-lib diagrams-svg;
  };

  diagrams-cairo = import ./diagrams-cairo-0.6.nix {
    inherit cabal cairo cmdargs colour diagrams-core diagrams-lib filepath mtl split time;
  };

  diagrams-core = import ./diagrams-core-0.6.0.1.nix {
    inherit cabal dual-tree MemoTrie monoid-extras newtype semigroups vector-space vector-space-points;
  };

  diagrams-gtk = import ./diagrams-gtk-0.6.nix {
    inherit cabal diagrams-cairo diagrams-lib gtk;
  };

  diagrams-lib = import ./diagrams-lib-0.6.0.1.nix {
    inherit cabal active colour data-default diagrams-core monoid-extras newtype NumInstances semigroups vector-space;
  };

  diagrams-svg = import ./diagrams-svg-0.6.0.1.nix {
    inherit cabal blaze-svg cmdargs colour diagrams-core diagrams-lib filepath monoid-extras mtl split time vector-space;
  };

  Diff = import ./Diff-0.2.0.nix {
    inherit cabal;
  };

  digest = import ./digest-0.0.1.0.nix {
    inherit cabal;
    inherit (nixpkgs) zlib;
  };

  dimensional = import ./dimensional-0.10.1.2.nix {
    inherit cabal numtype time;
  };

  directory-tree = import ./directory-tree-0.10.0.nix {
    inherit cabal filepath;
  };

  distributive = import ./distributive-0.3.nix {
    inherit cabal doctest filepath transformers transformers-compat;
  };

  dlist = import ./dlist-0.5.nix {
    inherit cabal;
  };

  doctest = import ./doctest-0.9.6.nix {
    inherit cabal base-compat deepseq filepath ghc-paths hspec HUnit QuickCheck setenv silently stringbuilder syb transformers;
  };

  download-curl = import ./download-curl-0.1.4.nix {
    inherit cabal curl feed tagsoup xml;
  };

  dpkg = import ./dpkg-0.0.3.nix {
    inherit cabal bindings-DSL HUnit libdpkg monad-loops;
  };

  dual-tree = import ./dual-tree-0.1.0.1.nix {
    inherit cabal monoid-extras newtype semigroups;
  };

  dyre = import ./dyre-0.8.9.nix {
    inherit cabal binary filepath ghc-paths io-storage time xdg-basedir;
  };

  EdisonAPI = import ./EdisonAPI-1.2.2.nix {
    inherit cabal mtl;
  };

  EdisonCore = import ./EdisonCore-1.2.2.nix {
    inherit cabal EdisonAPI mtl QuickCheck;
  };

  edit-distance = import ./edit-distance-0.2.1.1.nix {
    inherit cabal random;
  };

  editline = import ./editline-0.2.1.1.nix {
    inherit cabal;
    inherit (nixpkgs) libedit;
  };

  either = import ./either-3.4.nix {
    inherit cabal MonadRandom mtl semigroupoids semigroups transformers;
  };

  ekg = import ./ekg-0.3.1.2.nix {
    inherit cabal aeson filepath snap-core snap-server text time transformers unordered-containers;
  };

  email-validate = import ./email-validate-1.0.0.nix {
    inherit cabal attoparsec HUnit QuickCheck test-framework test-framework-hunit test-framework-quickcheck2;
  };

  entropy = import ./entropy-0.2.1.nix {
    inherit cabal;
  };

  enumerator = import ./enumerator-0.4.19.nix {
    inherit cabal text transformers;
  };

  erf = import ./erf-2.0.0.0.nix {
    inherit cabal;
  };

  errors = import ./errors-1.4.1.nix {
    inherit cabal either safe transformers;
  };

  event-list = import ./event-list-0.1.0.2.nix {
    inherit cabal non-negative QuickCheck transformers utility-ht;
  };

  exception-transformers = import ./exception-transformers-0.3.0.3.nix {
    inherit cabal HUnit stm transformers;
  };

  executable-path = import ./executable-path-0.0.3.nix {
    inherit cabal filepath;
  };

  explicit-exception = import ./explicit-exception-0.1.7.1.nix {
    inherit cabal transformers;
  };

  extensible-exceptions = import ./extensible-exceptions-0.1.1.4.nix {
    inherit cabal;
  };

  failure = import ./failure-0.2.0.1.nix {
    inherit cabal transformers;
  };

  fastcgi = import ./fastcgi-3001.0.2.4.nix {
    inherit cabal cgi;
    inherit (nixpkgs) fcgi;
  };

  fast-logger = import ./fast-logger-0.3.1.nix {
    inherit cabal blaze-builder date-cache filepath hspec text unix-time;
  };

  fclabels = import ./fclabels-1.1.4.3.nix {
    inherit cabal mtl transformers;
  };

  feed = import ./feed-0.3.8.nix {
    inherit cabal utf8-string xml;
  };

  fgl = import ./fgl-5.4.2.4.nix {
    inherit cabal mtl;
  };

  file-embed = import ./file-embed-0.0.4.6.nix {
    inherit cabal;
  };

  file-location = import ./file-location-0.4.5.1.nix {
    inherit cabal lifted-base transformers;
  };

  filemanip = import ./filemanip-0.3.6.2.nix {
    inherit cabal filepath mtl unix-compat;
  };

  filestore = import ./filestore-0.6.nix {
    inherit cabal Diff filepath HUnit mtl parsec split time utf8-string xml;
  };

  filesystem-conduit = import ./filesystem-conduit-1.0.0.nix {
    inherit cabal blaze-builder conduit hspec QuickCheck system-fileio system-filepath text transformers;
  };

  fingertree = import ./fingertree-0.0.1.1.nix {
    inherit cabal;
  };

  free = import ./free-3.3.1.nix {
    inherit cabal bifunctors comonad comonad-transformers comonads-fd distributive mtl semigroupoids semigroups transformers;
  };

  fsnotify = import ./fsnotify-0.0.11.nix {
    inherit cabal Cabal Glob hinotify hspec QuickCheck random system-fileio system-filepath text time uniqueid;
  };

  ftphs = import ./ftphs-1.0.8.nix {
    inherit cabal hslogger MissingH mtl network parsec regex-compat;
  };

  gconf = import ./gconf-0.12.1.1.nix {
    inherit cabal glib;
    inherit (nixpkgs) gconf gtk2hsBuildtools;
  };

  gd = import ./gd-3000.7.3.nix {
    inherit cabal;
    inherit (nixpkgs) expat fontconfig freetype gd libjpeg libpng zlib;
  };

  generic-deriving = import ./generic-deriving-1.4.0.nix {
    inherit cabal;
  };

  geniplate = import ./geniplate-0.6.0.3.nix {
    inherit cabal mtl;
  };

  ghc-events = import ./ghc-events-0.4.2.0.nix {
    inherit cabal binary mtl;
  };

  ghc-mtl = import ./ghc-mtl-1.0.1.2.nix {
    inherit cabal MonadCatchIO-mtl mtl;
  };

  ghc-paths = import ./ghc-paths-0.1.0.9.nix {
    inherit cabal;
  };

  ghc-syb-utils = import ./ghc-syb-utils-0.2.1.1.nix {
    inherit cabal syb;
  };

  gio = import ./gio-0.12.4.nix {
    inherit cabal glib mtl;
    inherit (nixpkgs) gtk2hsBuildtools;
  };

  github = import ./github-0.7.0.nix {
    inherit cabal aeson attoparsec case-insensitive conduit data-default failure HTTP http-conduit http-types network text time unordered-containers vector;
  };

  gitit = import ./gitit-0.10.3.1.nix {
    inherit cabal base64-bytestring blaze-html cgi ConfigFile feed filepath filestore ghc-paths happstack-server highlighting-kate hslogger HStringTemplate HTTP json mtl network pandoc pandoc-types parsec random recaptcha safe SHA syb tagsoup text time url utf8-string xhtml xml xss-sanitize zlib;
  };

  glade = import ./glade-0.12.1.nix {
    inherit cabal glib gtk;
    inherit (nixpkgs) gtk2hsBuildtools gtkC libc libglade pkgconfig;
  };

  GLFW = import ./GLFW-0.5.1.0.nix {
    inherit cabal OpenGL;
    inherit (nixpkgs) libX11 mesa;
  };

  glib = import ./glib-0.12.4.nix {
    inherit cabal;
    inherit (nixpkgs) glib gtk2hsBuildtools libc pkgconfig;
  };

  gloss = import ./gloss-1.7.8.3.nix {
    inherit cabal bmp GLUT OpenGL;
  };

  GLURaw = import ./GLURaw-1.3.0.0.nix {
    inherit cabal OpenGLRaw;
    inherit (nixpkgs) freeglut mesa;
  };

  GLUT = import ./GLUT-2.4.0.0.nix {
    inherit cabal OpenGL OpenGLRaw;
    inherit (nixpkgs) freeglut libICE libSM libXi libXmu mesa;
  };

  gnuidn = import ./gnuidn-0.2.nix {
    inherit cabal text;
    inherit (nixpkgs) c2hs libidn;
  };

  gnutls = import ./gnutls-0.1.4.nix {
    inherit cabal monads-tf transformers;
    inherit (nixpkgs) gnutls;
  };

  groupoids = import ./groupoids-3.0.1.1.nix {
    inherit cabal semigroupoids;
  };

  gsasl = import ./gsasl-0.3.5.nix {
    inherit cabal transformers;
    inherit (nixpkgs) gsasl;
  };

  gstreamer = import ./gstreamer-0.12.1.2.nix {
    inherit cabal glib mtl;
    inherit (nixpkgs) gstreamer gstreamer-audio gstreamer-base gstreamer-controller gstreamer-dataprotocol gstreamer-net gstreamer-plugins-base gtk2hsBuildtools;
  };

  gtk = import ./gtk-0.12.4.nix {
    inherit cabal cairo glib mtl pango;
    inherit (nixpkgs) gtk gtk2hsBuildtools libc pkgconfig;
  };

  gtk2hs-buildtools = import ./gtk2hs-buildtools-0.12.3.nix {
    inherit cabal filepath random;
    inherit (nixpkgs) alex happy;
  };

  gtkglext = import ./gtkglext-0.12.1.nix {
    inherit cabal glib gtk pango;
    inherit (nixpkgs) gtk2hsBuildtools gtkglext;
  };

  gtksourceview2 = import ./gtksourceview2-0.12.3.1.nix {
    inherit cabal glib gtk mtl;
    inherit (nixpkgs) gtk2hsBuildtools gtksourceview libc pkgconfig;
  };

  haddock = import ./haddock-2.13.1.nix {
    inherit cabal Cabal deepseq filepath ghc-paths xhtml;
    inherit (nixpkgs) alex happy;
  };

  hakyll = import ./hakyll-4.3.0.0.nix {
    inherit cabal binary blaze-html blaze-markup citeproc-hs cmdargs cryptohash data-default deepseq filepath fsnotify http-conduit http-types HUnit lrucache mtl pandoc parsec QuickCheck random regex-base regex-tdfa snap-core snap-server system-filepath tagsoup test-framework test-framework-hunit test-framework-quickcheck2 text time;
  };

  hamlet = import ./hamlet-1.1.7.1.nix {
    inherit cabal blaze-builder blaze-html blaze-markup failure hspec HUnit parsec shakespeare text;
  };

  happstack = import ./happstack-7.0.1.nix {
    inherit cabal happstack-server;
  };

  happstack-server = import ./happstack-server-7.1.7.nix {
    inherit cabal base64-bytestring blaze-html extensible-exceptions filepath hslogger html HUnit monad-control mtl network parsec sendfile syb system-filepath text threads time time-compat transformers transformers-base utf8-string xhtml zlib;
  };

  happy = import ./happy-1.18.10.nix {
    inherit cabal mtl;
    inherit (nixpkgs) perl;
  };

  harp = import ./harp-0.4.nix {
    inherit cabal;
  };

  hashable = import ./hashable-1.1.2.5.nix {
    inherit cabal QuickCheck random test-framework test-framework-quickcheck2 text;
  };

  hashed-storage = import ./hashed-storage-0.5.10.nix {
    inherit cabal binary dataenc extensible-exceptions filepath mmap mtl zlib;
  };

  hashmap = import ./hashmap-1.3.0.1.nix {
    inherit cabal deepseq hashable;
  };

  hashtables = import ./hashtables-1.0.1.8.nix {
    inherit cabal hashable primitive vector;
  };

  haskeline = import ./haskeline-0.7.0.3.nix {
    inherit cabal filepath terminfo transformers utf8String;
  };

  haskelldb = import ./haskelldb-2.2.2.nix {
    inherit cabal mtl time;
  };

  haskelldb-hdbc = import ./haskelldb-hdbc-2.2.2.nix {
    inherit cabal convertible haskelldb HDBC mtl;
  };

  haskelldb-hdbc-odbc = import ./haskelldb-hdbc-odbc-2.1.2.nix {
    inherit cabal haskelldb haskelldb-hdbc HDBC HDBC-odbc mtl;
  };

  haskelldb-hdbc-postgresql = import ./haskelldb-hdbc-postgresql-2.1.2.nix {
    inherit cabal haskelldb haskelldb-hdbc HDBC HDBC-postgresql mtl;
    inherit (nixpkgs) postgresql;
  };

  haskelldb-hdbc-sqlite3 = import ./haskelldb-hdbc-sqlite3-2.1.2.nix {
    inherit cabal haskelldb haskelldb-hdbc HDBC HDBC-sqlite3 mtl;
  };

  HaskellForMaths = import ./HaskellForMaths-0.4.3.nix {
    inherit cabal random;
  };

  haskell-lexer = import ./haskell-lexer-1.0.nix {
    inherit cabal;
  };

  haskell-src = import ./haskell-src-1.0.1.5.nix {
    inherit cabal syb;
    inherit (nixpkgs) happy;
  };

  haskell-src-exts = import ./haskell-src-exts-1.13.5.nix {
    inherit cabal cpphs;
    inherit (nixpkgs) happy;
  };

  haskore = import ./haskore-0.2.0.3.nix {
    inherit cabal data-accessor event-list haskell-src markov-chain midi non-negative parsec random transformers utility-ht;
  };

  hastache = import ./hastache-0.3.3.nix {
    inherit cabal blaze-builder filepath ieee754 mtl syb text utf8-string;
  };

  HaXml = import ./HaXml-1.23.3.nix {
    inherit cabal filepath polyparse random;
  };

  haxr = import ./haxr-3000.9.0.1.nix {
    inherit cabal base64-bytestring blaze-builder HaXml HTTP mtl network time utf8-string;
  };

  hbro = import ./hbro-1.1.2.0.nix {
    inherit cabal cond data-default dyre filepath glib gtk lens monad-control mtl network pango transformers transformers-base webkit xdg-basedir zeromq3-haskell;
  };

  hbro-contrib = import ./hbro-contrib-1.1.1.0.nix {
    inherit cabal filepath glib gtk hbro monad-control mtl network pango text time transformers-base webkit;
  };

  HCard = import ./HCard-0.0.nix {
    inherit cabal mtl QuickCheck random random-shuffle;
  };

  hcwiid = import ./hcwiid-0.0.1.nix {
    inherit cabal;
    inherit (nixpkgs) bluetooth cwiid;
  };

  HDBC = import ./HDBC-2.3.1.2.nix {
    inherit cabal convertible mtl text time utf8-string;
  };

  HDBC-odbc = import ./HDBC-odbc-2.3.1.1.nix {
    inherit cabal HDBC mtl time utf8-string;
    inherit (nixpkgs) odbc;
  };

  HDBC-postgresql = import ./HDBC-postgresql-2.3.2.1.nix {
    inherit cabal convertible HDBC mtl parsec time utf8-string;
    inherit (nixpkgs) postgresql;
  };

  HDBC-sqlite3 = import ./HDBC-sqlite3-2.3.3.0.nix {
    inherit cabal HDBC mtl utf8-string;
    inherit (nixpkgs) sqlite;
  };

  hedis = import ./hedis-0.6.2.nix {
    inherit cabal attoparsec BoundedChan bytestring-lexing HUnit mtl network resource-pool test-framework test-framework-hunit time vector;
  };

  HFuse = import ./HFuse-0.2.4.1.nix {
    inherit cabal;
    inherit (nixpkgs) fuse;
  };

  highlighting-kate = import ./highlighting-kate-0.5.3.8.nix {
    inherit cabal blaze-html filepath mtl parsec regex-pcre;
  };

  hinotify = import ./hinotify-0.3.5.nix {
    inherit cabal;
  };

  hint = import ./hint-0.3.3.6.nix {
    inherit cabal extensible-exceptions filepath ghc-mtl ghc-paths haskell-src MonadCatchIO-mtl mtl random utf8-string;
  };

  Hipmunk = import ./Hipmunk-5.2.0.10.nix {
    inherit cabal StateVar transformers;
  };

  HJavaScript = import ./HJavaScript-0.4.7.nix {
    inherit cabal;
  };

  HJScript = import ./HJScript-0.6.1.nix {
    inherit cabal HJavaScript hsx mtl;
  };

  hjsmin = import ./hjsmin-0.1.4.1.nix {
    inherit cabal blaze-builder Cabal HUnit language-javascript QuickCheck test-framework test-framework-hunit text;
  };

  hledger = import ./hledger-0.20.nix {
    inherit cabal cmdargs filepath haskeline hledger-lib HUnit mtl parsec regexpr safe shakespeare-text split text time utf8-string;
  };

  hledger-interest = import ./hledger-interest-1.4.2.nix {
    inherit cabal Cabal hledger-lib mtl time;
  };

  hledger-lib = import ./hledger-lib-0.20.nix {
    inherit cabal cmdargs csv filepath HUnit mtl parsec pretty-show regex-compat regexpr safe split time transformers utf8-string;
  };

  hledger-web = import ./hledger-web-0.20.nix {
    inherit cabal blaze-html blaze-markup clientsession cmdargs data-default filepath hamlet hjsmin hledger hledger-lib http-conduit HUnit monad-control network-conduit parsec regexpr safe shakespeare-css shakespeare-js shakespeare-text text time transformers wai wai-extra wai-handler-launch warp yaml yesod yesod-core yesod-default yesod-form yesod-static yesod-test;
  };

  hlint = import ./hlint-1.8.43.nix {
    inherit cabal cpphs filepath haskell-src-exts hscolour transformers uniplate;
  };

  hmatrix = import ./hmatrix-0.14.1.0.nix {
    inherit cabal binary random storable-complex vector;
    inherit (nixpkgs) blas gsl liblapack;
  };

  hoauth = import ./hoauth-0.3.5.nix {
    inherit cabal binary crypto-pubkey-types curl dataenc entropy mtl random RSA SHA time utf8-string;
  };

  hoogle = import ./hoogle-4.2.16.nix {
    inherit cabal binary blaze-builder Cabal case-insensitive cmdargs conduit filepath haskell-src-exts http-types parsec random safe tagsoup time transformers uniplate wai warp;
  };

  hOpenPGP = import ./hOpenPGP-0.7.nix {
    inherit cabal attoparsec base64-bytestring bzlib cereal cereal-conduit conduit crypto-pubkey cryptohash exception-transformers HUnit ixset lens mtl openpgp-asciiarmor resourcet split test-framework test-framework-hunit time zlib;
  };

  hostname = import ./hostname-1.0.nix {
    inherit cabal;
  };

  hS3 = import ./hS3-0.5.7.nix {
    inherit cabal Crypto dataenc HTTP hxt MissingH network random regex-compat utf8-string;
  };

  hs-bibutils = import ./hs-bibutils-4.12.nix {
    inherit cabal syb;
  };

  hscolour = import ./hscolour-1.20.3.nix {
    inherit cabal;
  };

  hscurses = import ./hscurses-1.4.1.2.nix {
    inherit cabal mtl;
  };

  hsemail = import ./hsemail-1.7.2.nix {
    inherit cabal mtl parsec;
  };

  HSH = import ./HSH-2.1.0.nix {
    inherit cabal filepath hslogger MissingH mtl regex-base regex-compat regex-posix;
  };

  hslogger = import ./hslogger-1.2.1.nix {
    inherit cabal mtl network time;
  };

  hsmagick = import ./hsmagick-0.5.nix {
    inherit cabal filepath;
    inherit (nixpkgs) bz2 freetype2 GraphicsMagick jasper lcms libjpeg libpng libxml2 tiff wmflite zlib;
  };

  hsp = import ./hsp-0.8.0.nix {
    inherit cabal mtl text;
  };

  hspec = import ./hspec-1.4.4.nix {
    inherit cabal ansi-terminal doctest filepath ghc-paths hspec-expectations hspec-meta HUnit QuickCheck setenv silently time transformers;
  };

  hspec-expectations = import ./hspec-expectations-0.3.0.3.nix {
    inherit cabal hspec HUnit markdown-unlit silently;
  };

  hsql = import ./hsql-1.8.2.nix {
    inherit cabal;
  };

  hsql-mysql = import ./hsql-mysql-1.8.2.nix {
    inherit cabal Cabal hsql;
    inherit (nixpkgs) mysqlclient;
  };

  hsql-odbc = import ./hsql-odbc-1.8.2.nix {
    inherit cabal hsql;
    inherit (nixpkgs) odbc;
  };

  hsql-postgresql = import ./hsql-postgresql-1.8.2.nix {
    inherit cabal hsql;
    inherit (nixpkgs) postgresql;
  };

  hsql-sqlite3 = import ./hsql-sqlite3-1.8.2.nix {
    inherit cabal hsql;
    inherit (nixpkgs) sqlite;
  };

  HsSyck = import ./HsSyck-0.50.nix {
    inherit cabal syb utf8-string;
  };

  HStringTemplate = import ./HStringTemplate-0.6.12.nix {
    inherit cabal blaze-builder deepseq filepath mtl parsec syb syb-with-class text time utf8-string;
  };

  hsx = import ./hsx-0.10.4.nix {
    inherit cabal haskell-src-exts mtl utf8-string;
  };

  html = import ./html-1.0.1.2.nix {
    inherit cabal;
  };

  html-conduit = import ./html-conduit-1.1.0.nix {
    inherit cabal conduit filesystem-conduit hspec HUnit resourcet system-filepath tagstream-conduit text transformers xml-conduit xml-types;
  };

  HTTP = import ./HTTP-4000.2.8.nix {
    inherit cabal case-insensitive conduit deepseq http-types httpd-shed HUnit mtl network parsec pureMD5 split test-framework test-framework-hunit wai warp;
  };

  http-conduit = import ./http-conduit-1.9.0.nix {
    inherit cabal asn1-data base64-bytestring blaze-builder blaze-builder-conduit case-insensitive certificate conduit cookie cprng-aes data-default deepseq failure filepath hspec http-types HUnit lifted-base mime-types monad-control mtl network network-conduit publicsuffixlist random regex-compat resourcet socks text time tls tls-extra transformers transformers-base utf8-string void wai warp zlib-conduit;
  };

  http-date = import ./http-date-0.0.2.nix {
    inherit cabal attoparsec HUnit test-framework-doctest test-framework-hunit test-framework-th-prime time;
  };

  http-reverse-proxy = import ./http-reverse-proxy-0.1.1.4.nix {
    inherit cabal blaze-builder case-insensitive classy-prelude conduit data-default hspec http-conduit http-types lifted-base monad-control network network-conduit text transformers wai warp word8;
  };

  http-types = import ./http-types-0.8.0.nix {
    inherit cabal blaze-builder case-insensitive hspec QuickCheck text;
  };

  HUnit = import ./HUnit-1.2.5.2.nix {
    inherit cabal deepseq;
  };

  hxt = import ./hxt-9.3.1.1.nix {
    inherit cabal binary deepseq filepath HUnit hxt-charproperties hxt-regex-xmlschema hxt-unicode mtl network parsec;
  };

  hxt-cache = import ./hxt-cache-9.1.0.nix {
    inherit cabal binary deepseq filepath hxt SHA time;
  };

  hxt-charproperties = import ./hxt-charproperties-9.1.1.nix {
    inherit cabal;
  };

  hxt-curl = import ./hxt-curl-9.1.1.nix {
    inherit cabal curl hxt parsec;
  };

  hxt-http = import ./hxt-http-9.1.4.nix {
    inherit cabal HTTP hxt network parsec;
  };

  hxt-regex-xmlschema = import ./hxt-regex-xmlschema-9.0.4.nix {
    inherit cabal hxt-charproperties parsec;
  };

  hxt-relaxng = import ./hxt-relaxng-9.1.4.nix {
    inherit cabal hxt hxt-charproperties hxt-regex-xmlschema network parsec;
  };

  hxt-tagsoup = import ./hxt-tagsoup-9.1.1.nix {
    inherit cabal hxt hxt-charproperties hxt-unicode tagsoup;
  };

  hxt-unicode = import ./hxt-unicode-9.0.2.nix {
    inherit cabal hxt-charproperties;
  };

  hxt-xpath = import ./hxt-xpath-9.1.2.nix {
    inherit cabal filepath hxt parsec;
  };

  hxt-xslt = import ./hxt-xslt-9.1.1.nix {
    inherit cabal filepath hxt hxt-xpath parsec;
  };

  iconv = import ./iconv-0.4.1.1.nix {
    inherit cabal;
  };

  idna = import ./idna-0.2.nix {
    inherit cabal punycode stringprep text;
  };

  ieee754 = import ./ieee754-0.7.3.nix {
    inherit cabal;
  };

  IfElse = import ./IfElse-0.85.nix {
    inherit cabal mtl;
  };

  io-choice = import ./io-choice-0.0.1.nix {
    inherit cabal lifted-base monad-control transformers transformers-base;
  };

  IOSpec = import ./IOSpec-0.2.5.nix {
    inherit cabal mtl QuickCheck Stream;
  };

  io-storage = import ./io-storage-0.3.nix {
    inherit cabal;
  };

  irc = import ./irc-0.5.1.0.nix {
    inherit cabal parsec;
  };

  ircbot = import ./ircbot-0.5.3.nix {
    inherit cabal filepath irc mtl network parsec random stm time;
  };

  iteratee = import ./iteratee-0.8.9.4.nix {
    inherit cabal ListLike monad-control MonadCatchIO-transformers parallel transformers transformers-base;
  };

  ixset = import ./ixset-1.0.3.nix {
    inherit cabal Cabal HUnit QuickCheck random safecopy syb syb-with-class;
  };

  json = import ./json-0.5.nix {
    inherit cabal mtl parsec syb;
  };

  keys = import ./keys-3.0.2.nix {
    inherit cabal comonad-transformers comonads-fd free semigroupoids semigroups transformers;
  };

  knob = import ./knob-0.1.1.nix {
    inherit cabal transformers;
  };

  lambdabot = import ./lambdabot-4.3.nix {
    inherit cabal arrows binary brainfuck data-memocombinators dependent-map dependent-sum dependent-sum-template dice edit-distance filepath haskeline haskell-src-exts hoogle hslogger hstatsd HTTP IOSpec lifted-base logict misfortune monad-control MonadRandom mtl mueval network numbers oeis parsec QuickCheck random random-fu random-source regex-tdfa SafeSemaphore show split syb tagsoup time transformers transformers-base unlambda utf8-string vector-space zlib;
  };

  lambdabot-utils = import ./lambdabot-utils-4.2.2.nix {
    inherit cabal binary haskell-src mtl network random regex-compat regex-posix syb tagsoup utf8-string zlib;
  };

  language-c = import ./language-c-0.4.2.nix {
    inherit cabal filepath syb;
    inherit (nixpkgs) alex happy;
  };

  language-haskell-extract = import ./language-haskell-extract-0.2.1.nix {
    inherit cabal haskell-src-exts regex-posix;
  };

  language-javascript = import ./language-javascript-0.5.6.nix {
    inherit cabal blaze-builder Cabal HUnit mtl QuickCheck test-framework test-framework-hunit utf8-light utf8-string;
    inherit (nixpkgs) happy;
  };

  largeword = import ./largeword-1.0.5.nix {
    inherit cabal HUnit QuickCheck test-framework test-framework-hunit test-framework-quickcheck2;
  };

  lazysmallcheck = import ./lazysmallcheck-0.6.nix {
    inherit cabal;
  };

  LDAP = import ./LDAP-0.6.9.nix {
    inherit cabal;
    inherit (nixpkgs) lber ldap;
  };

  leksah = import ./leksah-0.12.1.3.nix {
    inherit cabal binary binary-shared Cabal deepseq enumerator filepath gio glib gtk gtksourceview2 hslogger leksah-server ltk mtl network parsec QuickCheck regex-base regex-tdfa strict text time transformers utf8-string;
  };

  leksah-server = import ./leksah-server-0.12.1.2.nix {
    inherit cabal attoparsec attoparsec-enumerator binary binary-shared Cabal deepseq enumerator filepath haddock hslogger HUnit ltk network parsec process-leksah strict time transformers;
  };

  lens = import ./lens-3.9.0.2.nix {
    inherit cabal bifunctors comonad comonad-transformers comonads-fd contravariant deepseq distributive doctest filepath generic-deriving hashable HUnit MonadCatchIO-transformers mtl nats parallel profunctor-extras profunctors QuickCheck reflection semigroupoids semigroups simple-reflect split tagged test-framework test-framework-hunit test-framework-quickcheck2 test-framework-th text transformers transformers-compat unordered-containers vector void;
  };

  lhs2tex = import ./lhs2tex-1.18.1.nix {
    inherit cabal filepath mtl regex-compat;
  };

  libtagc = import ./libtagc-0.12.0.nix {
    inherit cabal glib;
    inherit (nixpkgs) tag_c taglib_c;
  };

  libxml-sax = import ./libxml-sax-0.7.3.nix {
    inherit cabal text xml-types;
    inherit (nixpkgs) libxml2;
  };

  LibZip = import ./LibZip-0.10.2.nix {
    inherit cabal bindings-libzip filepath mtl time;
  };

  lifted-base = import ./lifted-base-0.2.0.3.nix {
    inherit cabal base-unicode-symbols HUnit monad-control test-framework test-framework-hunit transformers transformers-base;
  };

  ListLike = import ./ListLike-3.1.7.1.nix {
    inherit cabal;
  };

  llvm = import ./llvm-3.2.0.0.nix {
    inherit cabal llvm-base mtl type-level;
  };

  llvm-base = import ./llvm-base-3.2.0.0.nix {
    inherit cabal mtl;
    inherit (nixpkgs) llvm;
  };

  logict = import ./logict-0.5.0.2.nix {
    inherit cabal mtl;
  };

  lrucache = import ./lrucache-1.1.1.3.nix {
    inherit cabal;
  };

  ltk = import ./ltk-0.12.1.0.nix {
    inherit cabal Cabal filepath glib gtk mtl parsec transformers;
  };

  maccatcher = import ./maccatcher-2.1.5.nix {
    inherit cabal binary parsec;
  };

  magic = import ./magic-1.0.8.nix {
    inherit cabal;
    inherit (nixpkgs) magic;
  };

  markov-chain = import ./markov-chain-0.0.3.2.nix {
    inherit cabal random transformers;
  };

  math-functions = import ./math-functions-0.1.1.0.nix {
    inherit cabal erf HUnit ieee754 QuickCheck test-framework test-framework-hunit test-framework-quickcheck2 vector;
  };

  MaybeT = import ./MaybeT-0.1.2.nix {
    inherit cabal mtl;
  };

  mbox = import ./mbox-0.1.nix {
    inherit cabal safe time;
  };

  MemoTrie = import ./MemoTrie-0.6.1.nix {
    inherit cabal void;
  };

  mersenne-random = import ./mersenne-random-1.0.0.1.nix {
    inherit cabal;
  };

  mersenne-random-pure64 = import ./mersenne-random-pure64-0.2.0.3.nix {
    inherit cabal random;
  };

  midi = import ./midi-0.2.0.1.nix {
    inherit cabal binary event-list explicit-exception monoid-transformer non-negative QuickCheck random transformers;
  };

  mighttpd2 = import ./mighttpd2-2.5.9.nix {
    inherit cabal deepseq filepath http-conduit http-date http-types HUnit network network-conduit parsec test-framework-hunit test-framework-th-prime time transformers unix-bytestring unordered-containers wai wai-app-file-cgi wai-logger wai-logger-prefork warp;
  };

  mime-mail = import ./mime-mail-0.4.1.2.nix {
    inherit cabal base64-bytestring blaze-builder filepath random text;
  };

  mime-types = import ./mime-types-0.1.0.0.nix {
    inherit cabal text;
  };

  MissingH = import ./MissingH-1.2.0.0.nix {
    inherit cabal filepath hslogger HUnit mtl network parsec random regex-compat time;
  };

  mmap = import ./mmap-0.5.8.nix {
    inherit cabal;
  };

  mmorph = import ./mmorph-1.0.0.nix {
    inherit cabal transformers;
  };

  MonadCatchIO-mtl = import ./MonadCatchIO-mtl-0.3.0.5.nix {
    inherit cabal extensible-exceptions mtl;
  };

  MonadCatchIO-transformers = import ./MonadCatchIO-transformers-0.3.0.0.nix {
    inherit cabal extensible-exceptions transformers;
  };

  monad-control = import ./monad-control-0.3.1.4.nix {
    inherit cabal base-unicode-symbols transformers transformers-base;
  };

  monadcryptorandom = import ./monadcryptorandom-0.5.nix {
    inherit cabal crypto-api mtl tagged transformers;
  };

  monad-logger = import ./monad-logger-0.3.0.1.nix {
    inherit cabal conduit fast-logger monad-control mtl resourcet text transformers transformers-base;
  };

  monad-loops = import ./monad-loops-0.3.2.0.nix {
    inherit cabal stm;
  };

  monad-par = import ./monad-par-0.1.0.3.nix {
    inherit cabal deepseq HUnit;
  };

  MonadPrompt = import ./MonadPrompt-1.0.0.3.nix {
    inherit cabal mtl;
  };

  MonadRandom = import ./MonadRandom-0.1.6.nix {
    inherit cabal mtl random;
  };

  monads-tf = import ./monads-tf-0.1.0.1.nix {
    inherit cabal transformers;
  };

  monoid-extras = import ./monoid-extras-0.2.2.2.nix {
    inherit cabal semigroups;
  };

  monoid-transformer = import ./monoid-transformer-0.0.2.nix {
    inherit cabal;
  };

  mtl = import ./mtl-2.1.2.nix {
    inherit cabal transformers;
  };

  mtlparse = import ./mtlparse-0.1.2.nix {
    inherit cabal mtl;
  };

  mueval = import ./mueval-0.9.nix {
    inherit cabal Cabal extensible-exceptions filepath hint mtl show simple-reflect utf8-string;
  };

  murmur-hash = import ./murmur-hash-0.1.0.6.nix {
    inherit cabal;
  };

  mwc-random = import ./mwc-random-0.11.0.0.nix {
    inherit cabal HUnit primitive QuickCheck statistics test-framework test-framework-hunit test-framework-quickcheck2 time vector;
  };

  nats = import ./nats-0.1.nix {
    inherit cabal;
  };

  ncurses = import ./ncurses-0.2.1.nix {
    inherit cabal text transformers;
    inherit (nixpkgs) c2hs ncursesw panel;
  };

  netwire = import ./netwire-4.0.7.nix {
    inherit cabal bifunctors deepseq lifted-base monad-control mtl profunctors random semigroups tagged time vector-space;
  };

  network = import ./network-2.4.1.2.nix {
    inherit cabal HUnit parsec test-framework test-framework-hunit test-framework-quickcheck2;
  };

  network-conduit = import ./network-conduit-1.0.0.nix {
    inherit cabal conduit lifted-base monad-control network transformers;
  };

  network-info = import ./network-info-0.2.0.3.nix {
    inherit cabal;
  };

  network-multicast = import ./network-multicast-0.0.7.nix {
    inherit cabal network;
  };

  network-protocol-xmpp = import ./network-protocol-xmpp-0.4.4.nix {
    inherit cabal gnuidn gnutls gsasl libxml-sax monads-tf network text transformers xml-types;
  };

  newtype = import ./newtype-0.2.nix {
    inherit cabal;
  };

  non-negative = import ./non-negative-0.1.nix {
    inherit cabal QuickCheck utility-ht;
  };

  numbers = import ./numbers-3000.1.0.3.nix {
    inherit cabal QuickCheck test-framework test-framework-quickcheck2;
  };

  numeric-quest = import ./numeric-quest-0.2.nix {
    inherit cabal;
  };

  NumInstances = import ./NumInstances-1.0.nix {
    inherit cabal;
  };

  numtype = import ./numtype-1.0.nix {
    inherit cabal;
  };

  ObjectName = import ./ObjectName-1.0.0.0.nix {
    inherit cabal;
  };

  oeis = import ./oeis-0.3.1.nix {
    inherit cabal HTTP network;
  };

  ofx = import ./ofx-0.2.0.0.nix {
    inherit cabal explicit-exception parsec time;
  };

  OpenAL = import ./OpenAL-1.4.0.2.nix {
    inherit cabal ObjectName StateVar Tensor;
    inherit (nixpkgs) openal;
  };

  OpenGL = import ./OpenGL-2.8.0.0.nix {
    inherit cabal GLURaw OpenGLRaw;
    inherit (nixpkgs) libX11 mesa;
  };

  OpenGLRaw = import ./OpenGLRaw-1.3.0.0.nix {
    inherit cabal;
    inherit (nixpkgs) mesa;
  };

  openpgp = import ./openpgp-0.4.nix {
    inherit cabal binary bzlib HUnit QuickCheck quickcheck-instances test-framework test-framework-hunit test-framework-quickcheck2 utf8-string zlib;
  };

  openpgp-asciiarmor = import ./openpgp-asciiarmor-0.1.nix {
    inherit cabal attoparsec base64-bytestring cereal HUnit test-framework test-framework-hunit;
  };

  options = import ./options-0.1.1.nix {
    inherit cabal monads-tf system-filepath text transformers;
  };

  optparse-applicative = import ./optparse-applicative-0.5.2.1.nix {
    inherit cabal HUnit test-framework test-framework-hunit test-framework-th-prime transformers;
  };

  pandoc = import ./pandoc-1.11.1.nix {
    inherit cabal ansi-terminal base64-bytestring blaze-html blaze-markup citeproc-hs data-default Diff extensible-exceptions filepath highlighting-kate HTTP HUnit json mtl network pandoc-types parsec QuickCheck random syb tagsoup temporary test-framework test-framework-hunit test-framework-quickcheck2 texmath text time xml zip-archive zlib;
  };

  pandoc-types = import ./pandoc-types-1.10.nix {
    inherit cabal syb;
  };

  pango = import ./pango-0.12.4.nix {
    inherit cabal cairo glib mtl;
    inherit (nixpkgs) gtk2hsBuildtools libc pango pkgconfig;
  };

  parallel = import ./parallel-3.2.0.3.nix {
    inherit cabal deepseq;
  };

  parseargs = import ./parseargs-0.1.3.2.nix {
    inherit cabal;
  };

  parsec = import ./parsec-3.1.3.nix {
    inherit cabal mtl text;
  };

  pastis = import ./pastis-0.1.2.nix {
    inherit cabal HTTP network;
  };

  path-pieces = import ./path-pieces-0.1.2.nix {
    inherit cabal file-location hspec HUnit QuickCheck text time;
  };

  patience = import ./patience-0.1.1.nix {
    inherit cabal;
  };

  pcap = import ./pcap-0.4.5.2.nix {
    inherit cabal network time;
  };

  pcre-light = import ./pcre-light-0.4.nix {
    inherit cabal;
    inherit (nixpkgs) pcre;
  };

  pem = import ./pem-0.1.2.nix {
    inherit cabal attoparsec base64-bytestring cereal mtl QuickCheck test-framework test-framework-quickcheck2;
  };

  persistent = import ./persistent-1.1.5.1.nix {
    inherit cabal aeson attoparsec base64-bytestring blaze-html blaze-markup conduit hspec lifted-base monad-control monad-logger path-pieces pool-conduit resourcet silently text time transformers transformers-base unordered-containers vector;
    inherit (nixpkgs) sqlite;
  };

  persistent-postgresql = import ./persistent-postgresql-1.1.3.1.nix {
    inherit cabal aeson conduit monad-control persistent postgresql-libpq postgresql-simple text time transformers;
  };

  persistent-sqlite = import ./persistent-sqlite-1.1.5.nix {
    inherit cabal aeson conduit monad-control monad-logger persistent text transformers;
  };

  persistent-template = import ./persistent-template-1.1.2.4.nix {
    inherit cabal aeson hspec monad-control persistent QuickCheck text transformers;
  };

  pointedlist = import ./pointedlist-0.4.0.4.nix {
    inherit cabal binary derive fclabels;
  };

  polyparse = import ./polyparse-1.7.nix {
    inherit cabal text;
  };

  pool-conduit = import ./pool-conduit-0.1.1.nix {
    inherit cabal resource-pool resourcet transformers;
  };

  postgresql-libpq = import ./postgresql-libpq-0.8.2.1.nix {
    inherit cabal;
    inherit (nixpkgs) postgresql;
  };

  postgresql-simple = import ./postgresql-simple-0.2.4.1.nix {
    inherit cabal attoparsec base16-bytestring blaze-builder blaze-textual cryptohash HUnit postgresql-libpq text time transformers vector;
  };

  pretty-show = import ./pretty-show-1.2.nix {
    inherit cabal haskell-lexer;
  };

  primes = import ./primes-0.2.1.0.nix {
    inherit cabal;
  };

  primitive = import ./primitive-0.5.0.1.nix {
    inherit cabal;
  };

  process-conduit = import ./process-conduit-1.0.0.0.nix {
    inherit cabal conduit control-monad-loop hspec mtl shakespeare-text text;
  };

  process-extras = import ./process-extras-0.2.0.nix {
    inherit cabal deepseq text;
  };

  profunctor-extras = import ./profunctor-extras-3.3.nix {
    inherit cabal comonad profunctors semigroupoid-extras semigroupoids tagged transformers;
  };

  profunctors = import ./profunctors-3.3.nix {
    inherit cabal comonad tagged;
  };

  project-template = import ./project-template-0.1.3.nix {
    inherit cabal base64-bytestring base64-conduit classy-prelude conduit hspec mtl QuickCheck resourcet system-fileio system-filepath text transformers;
  };

  PSQueue = import ./PSQueue-1.1.nix {
    inherit cabal;
  };

  publicsuffixlist = import ./publicsuffixlist-0.1.nix {
    inherit cabal cereal data-default HUnit idna text utf8-string;
  };

  punycode = import ./punycode-2.0.nix {
    inherit cabal cereal encoding HUnit mtl QuickCheck text;
  };

  pureMD5 = import ./pureMD5-2.1.2.1.nix {
    inherit cabal binary cereal crypto-api tagged;
  };

  pwstore-fast = import ./pwstore-fast-2.3.nix {
    inherit cabal base64-bytestring cryptohash random;
  };

  QuickCheck = import ./QuickCheck-2.6.nix {
    inherit cabal random;
  };

  quickcheck-instances = import ./quickcheck-instances-0.3.2.nix {
    inherit cabal QuickCheck text time;
  };

  random = import ./random-1.0.1.1.nix {
    inherit cabal time;
  };

  random-shuffle = import ./random-shuffle-0.0.4.nix {
    inherit cabal MonadRandom random;
  };

  Ranged-sets = import ./Ranged-sets-0.3.0.nix {
    inherit cabal HUnit QuickCheck;
  };

  ranges = import ./ranges-0.2.4.nix {
    inherit cabal;
  };

  reactive-banana = import ./reactive-banana-0.7.1.2.nix {
    inherit cabal hashable HUnit test-framework test-framework-hunit transformers unordered-containers vault;
  };

  ReadArgs = import ./ReadArgs-1.2.1.nix {
    inherit cabal hspec system-filepath text;
  };

  readline = import ./readline-1.0.3.0.nix {
    inherit cabal;
  };

  recaptcha = import ./recaptcha-0.1.nix {
    inherit cabal HTTP network xhtml;
  };

  reflection = import ./reflection-1.2.0.1.nix {
    inherit cabal tagged;
  };

  reform = import ./reform-0.1.2.nix {
    inherit cabal mtl text;
  };

  reform-blaze = import ./reform-blaze-0.1.2.nix {
    inherit cabal blaze-html blaze-markup reform text;
  };

  reform-happstack = import ./reform-happstack-0.1.2.nix {
    inherit cabal happstack-server mtl random reform utf8-string;
  };

  regex-base = import ./regex-base-0.93.2.nix {
    inherit cabal mtl;
  };

  regex-compat = import ./regex-compat-0.95.1.nix {
    inherit cabal regex-base regex-posix;
  };

  regex-pcre = import ./regex-pcre-0.94.4.nix {
    inherit cabal regex-base;
    inherit (nixpkgs) pcre;
  };

  regex-posix = import ./regex-posix-0.95.2.nix {
    inherit cabal regex-base;
  };

  regexpr = import ./regexpr-0.5.4.nix {
    inherit cabal HUnit mtl mtlparse;
  };

  regex-tdfa = import ./regex-tdfa-1.1.8.nix {
    inherit cabal mtl parsec regex-base;
  };

  regex-tdfa-utf8 = import ./regex-tdfa-utf8-1.0.nix {
    inherit cabal regex-base regex-tdfa utf8-string;
  };

  repa = import ./repa-3.2.3.3.nix {
    inherit cabal QuickCheck vector;
  };

  representable-functors = import ./representable-functors-3.0.1.nix {
    inherit cabal comonad comonad-transformers comonads-fd contravariant distributive free keys mtl semigroupoids semigroups transformers;
  };

  representable-tries = import ./representable-tries-3.0.2.nix {
    inherit cabal adjunctions bifunctors comonad comonad-transformers distributive keys mtl representable-functors semigroupoids semigroups transformers;
  };

  resource-pool = import ./resource-pool-0.2.1.1.nix {
    inherit cabal hashable monad-control stm time transformers transformers-base vector;
  };

  resourcet = import ./resourcet-0.4.7.1.nix {
    inherit cabal hspec lifted-base mmorph monad-control mtl transformers transformers-base;
  };

  rosezipper = import ./rosezipper-0.2.nix {
    inherit cabal;
  };

  RSA = import ./RSA-1.2.2.0.nix {
    inherit cabal binary crypto-api crypto-pubkey-types monadcryptorandom pureMD5 SHA;
  };

  rss2irc = import ./rss2irc-1.0.3.nix {
    inherit cabal cabal-file-th cmdargs deepseq feed http-conduit http-types io-storage irc network parsec regexpr resourcet safe split text time transformers utf8-string;
  };

  safe = import ./safe-0.3.3.nix {
    inherit cabal;
  };

  safecopy = import ./safecopy-0.7.1.nix {
    inherit cabal cereal text time;
  };

  SafeSemaphore = import ./SafeSemaphore-0.9.0.nix {
    inherit cabal HUnit stm;
  };

  SDL = import ./SDL-0.6.4.nix {
    inherit cabal;
    inherit (nixpkgs) SDL;
  };

  SDL-gfx = import ./SDL-gfx-0.6.0.nix {
    inherit cabal SDL;
  };

  SDL-image = import ./SDL-image-0.6.1.nix {
    inherit cabal SDL;
    inherit (nixpkgs) SDL_image;
  };

  SDL-mixer = import ./SDL-mixer-0.6.1.nix {
    inherit cabal SDL;
    inherit (nixpkgs) SDL_mixer;
  };

  SDL-ttf = import ./SDL-ttf-0.6.2.nix {
    inherit cabal SDL;
    inherit (nixpkgs) SDL_ttf;
  };

  semigroupoid-extras = import ./semigroupoid-extras-3.0.1.nix {
    inherit cabal comonad distributive groupoids semigroupoids;
  };

  semigroupoids = import ./semigroupoids-3.0.2.nix {
    inherit cabal comonad contravariant semigroups transformers;
  };

  semigroups = import ./semigroups-0.9.nix {
    inherit cabal nats;
  };

  sendfile = import ./sendfile-0.7.9.nix {
    inherit cabal network;
  };

  setenv = import ./setenv-0.1.0.nix {
    inherit cabal hspec QuickCheck;
  };

  sfml-audio = import ./sfml-audio-0.7.0.1816.nix {
    inherit cabal;
    inherit (nixpkgs) libsndfile openal pthread;
  };

  SHA = import ./SHA-1.5.0.1.nix {
    inherit cabal binary;
  };

  shakespeare = import ./shakespeare-1.0.2.nix {
    inherit cabal hspec parsec text;
  };

  shakespeare-css = import ./shakespeare-css-1.0.2.nix {
    inherit cabal hspec HUnit parsec shakespeare text transformers;
  };

  shakespeare-i18n = import ./shakespeare-i18n-1.0.0.2.nix {
    inherit cabal parsec shakespeare text;
  };

  shakespeare-js = import ./shakespeare-js-1.1.0.nix {
    inherit cabal aeson hspec HUnit shakespeare text;
  };

  shakespeare-text = import ./shakespeare-text-1.0.0.5.nix {
    inherit cabal hspec HUnit shakespeare text;
  };

  Shellac = import ./Shellac-0.9.5.1.nix {
    inherit cabal mtl;
  };

  shelltestrunner = import ./shelltestrunner-1.3.1.nix {
    inherit cabal cabal-file-th cmdargs Diff filemanip filepath HUnit parsec regex-tdfa test-framework test-framework-hunit utf8-string;
  };

  show = import ./show-0.6.nix {
    inherit cabal syb;
  };

  silently = import ./silently-1.2.4.nix {
    inherit cabal deepseq nanospec;
  };

  SimpleEA = import ./SimpleEA-0.2.1.nix {
    inherit cabal mersenne-random-pure64 MonadRandom;
  };

  simpleirc = import ./simpleirc-0.2.1.nix {
    inherit cabal network time;
  };

  simple-reflect = import ./simple-reflect-0.3.1.nix {
    inherit cabal;
  };

  simple-sendfile = import ./simple-sendfile-0.2.11.nix {
    inherit cabal conduit hspec HUnit network network-conduit;
  };

  skein = import ./skein-1.0.3.nix {
    inherit cabal cereal crypto-api filepath hspec tagged;
  };

  smallcheck = import ./smallcheck-0.6.nix {
    inherit cabal dlist;
  };

  SMTPClient = import ./SMTPClient-1.0.4.nix {
    inherit cabal extensible-exceptions hsemail network;
  };

  snap-core = import ./snap-core-0.9.3.1.nix {
    inherit cabal attoparsec attoparsec-enumerator blaze-builder blaze-builder-enumerator bytestring-mmap case-insensitive deepseq enumerator filepath HUnit MonadCatchIO-transformers mtl random regex-posix text time unix-compat unordered-containers vector zlib-enum;
  };

  snap-server = import ./snap-server-0.9.3.1.nix {
    inherit cabal attoparsec attoparsec-enumerator blaze-builder blaze-builder-enumerator case-insensitive enumerator MonadCatchIO-transformers mtl network snap-core text time unix-compat;
  };

  socks = import ./socks-0.4.1.nix {
    inherit cabal cereal network;
  };

  split = import ./split-0.2.2.nix {
    inherit cabal QuickCheck;
  };

  StateVar = import ./StateVar-1.0.0.0.nix {
    inherit cabal;
  };

  static-hash = import ./static-hash-0.0.1.nix {
    inherit cabal hashable primes;
  };

  statistics = import ./statistics-0.10.2.0.nix {
    inherit cabal deepseq erf HUnit ieee754 math-functions monad-par mwc-random primitive QuickCheck test-framework test-framework-hunit test-framework-quickcheck2 vector vector-algorithms;
  };

  stm = import ./stm-2.4.2.nix {
    inherit cabal;
  };

  storable-complex = import ./storable-complex-0.2.1.nix {
    inherit cabal;
  };

  Stream = import ./Stream-0.4.6.nix {
    inherit cabal lazysmallcheck QuickCheck;
  };

  strict = import ./strict-0.3.2.nix {
    inherit cabal;
  };

  strict-concurrency = import ./strict-concurrency-0.2.4.1.nix {
    inherit cabal deepseq;
  };

  stringprep = import ./stringprep-0.1.5.nix {
    inherit cabal ranges text text-icu;
  };

  stringsearch = import ./stringsearch-0.3.6.4.nix {
    inherit cabal;
  };

  strptime = import ./strptime-1.0.6.nix {
    inherit cabal time;
  };

  svgcairo = import ./svgcairo-0.12.1.nix {
    inherit cabal cairo glib mtl;
    inherit (nixpkgs) gtk2hsBuildtools libc librsvg;
  };

  syb = import ./syb-0.4.0.nix {
    inherit cabal HUnit mtl;
  };

  syb-with-class = import ./syb-with-class-0.6.1.4.nix {
    inherit cabal;
  };

  syb-with-class-instances-text = import ./syb-with-class-instances-text-0.0.1.nix {
    inherit cabal syb-with-class text;
  };

  system-fileio = import ./system-fileio-0.3.10.nix {
    inherit cabal system-filepath text time;
  };

  system-filepath = import ./system-filepath-0.4.7.nix {
    inherit cabal deepseq text;
  };

  tagged = import ./tagged-0.6.nix {
    inherit cabal;
  };

  tagsoup = import ./tagsoup-0.12.8.nix {
    inherit cabal text;
  };

  tagstream-conduit = import ./tagstream-conduit-0.5.4.nix {
    inherit cabal attoparsec attoparsec-conduit blaze-builder blaze-builder-conduit case-insensitive conduit hspec HUnit QuickCheck text transformers;
  };

  tar = import ./tar-0.4.0.1.nix {
    inherit cabal filepath time;
  };

  template = import ./template-0.2.0.8.nix {
    inherit cabal mtl text;
  };

  temporary = import ./temporary-1.1.2.4.nix {
    inherit cabal filepath;
  };

  Tensor = import ./Tensor-1.0.0.1.nix {
    inherit cabal;
  };

  terminal-progress-bar = import ./terminal-progress-bar-0.0.1.3.nix {
    inherit cabal base-unicode-symbols HUnit test-framework test-framework-hunit;
  };

  terminfo = import ./terminfo-0.3.2.5.nix {
    inherit cabal;
    inherit (nixpkgs) ncurses;
  };

  test-framework = import ./test-framework-0.8.nix {
    inherit cabal ansi-terminal ansi-wl-pprint extensible-exceptions hostname random regex-posix time xml;
  };

  test-framework-hunit = import ./test-framework-hunit-0.3.0.nix {
    inherit cabal extensible-exceptions HUnit test-framework;
  };

  test-framework-quickcheck2 = import ./test-framework-quickcheck2-0.3.0.1.nix {
    inherit cabal extensible-exceptions QuickCheck random test-framework;
  };

  test-framework-th = import ./test-framework-th-0.2.2.nix {
    inherit cabal haskell-src-exts language-haskell-extract regex-posix test-framework;
  };

  test-framework-th-prime = import ./test-framework-th-prime-0.0.5.nix {
    inherit cabal cpphs haskell-src-exts test-framework;
  };

  texmath = import ./texmath-0.6.1.3.nix {
    inherit cabal parsec syb xml;
  };

  text = import ./text-0.11.3.1.nix {
    inherit cabal deepseq HUnit QuickCheck random test-framework test-framework-hunit test-framework-quickcheck2;
  };

  text-icu = import ./text-icu-0.6.3.5.nix {
    inherit cabal text;
    inherit (nixpkgs) icu;
  };

  th-extras = import ./th-extras-0.0.0.2.nix {
    inherit cabal syb;
  };

  threads = import ./threads-0.5.0.2.nix {
    inherit cabal base-unicode-symbols concurrent-extra HUnit stm test-framework test-framework-hunit;
  };

  threadscope = import ./threadscope-0.2.2.nix {
    inherit cabal binary cairo deepseq filepath ghc-events glib gtk mtl pango time;
  };

  time-compat = import ./time-compat-0.1.0.1.nix {
    inherit cabal time;
  };

  TinyURL = import ./TinyURL-0.1.0.nix {
    inherit cabal HTTP network;
  };

  tls = import ./tls-1.1.2.nix {
    inherit cabal cereal certificate cprng-aes crypto-pubkey crypto-random-api cryptohash mtl network QuickCheck test-framework test-framework-quickcheck2 time;
  };

  tls-extra = import ./tls-extra-0.6.3.nix {
    inherit cabal certificate cipher-aes cipher-rc4 crypto-pubkey crypto-random-api cryptohash mtl network pem text time tls vector;
  };

  tokyocabinet-haskell = import ./tokyocabinet-haskell-0.0.5.nix {
    inherit cabal mtl;
    inherit (nixpkgs) tokyocabinet;
  };

  transformers = import ./transformers-0.3.0.0.nix {
    inherit cabal;
  };

  transformers-base = import ./transformers-base-0.4.1.nix {
    inherit cabal transformers;
  };

  type-level = import ./type-level-0.2.4.nix {
    inherit cabal syb;
  };

  uniplate = import ./uniplate-1.6.7.nix {
    inherit cabal hashable syb unordered-containers;
  };

  unix-bytestring = import ./unix-bytestring-0.3.5.4.nix {
    inherit cabal;
  };

  unix-compat = import ./unix-compat-0.4.0.0.nix {
    inherit cabal;
  };

  unix-time = import ./unix-time-0.1.2.nix {
    inherit cabal hspec-expectations test-framework-hunit test-framework-th-prime;
  };

  Unixutils = import ./Unixutils-1.50.nix {
    inherit cabal filepath pureMD5 regex-tdfa zlib;
    inherit (nixpkgs) crypt;
  };

  unlambda = import ./unlambda-0.1.3.nix {
    inherit cabal mtl;
  };

  unordered-containers = import ./unordered-containers-0.2.3.0.nix {
    inherit cabal ChasingBottoms deepseq hashable HUnit QuickCheck test-framework test-framework-hunit test-framework-quickcheck2;
  };

  uri = import ./uri-0.1.6.nix {
    inherit cabal parsec safe utf8-string;
  };

  url = import ./url-2.1.2.nix {
    inherit cabal utf8-string;
  };

  utf8-light = import ./utf8-light-0.4.0.1.nix {
    inherit cabal;
  };

  utf8-string = import ./utf8-string-0.3.7.nix {
    inherit cabal;
  };

  utility-ht = import ./utility-ht-0.0.5.1.nix {
    inherit cabal;
  };

  uuagc = import ./uuagc-0.9.42.2.nix {
    inherit cabal filepath haskell-src-exts mtl uuagc-cabal uulib;
  };

  uuagc-cabal = import ./uuagc-cabal-1.0.4.0.nix {
    inherit cabal Cabal filepath mtl uulib;
  };

  uuid = import ./uuid-1.2.3.nix {
    inherit cabal binary criterion Crypto deepseq HUnit maccatcher mersenne-random-pure64 QuickCheck random time;
  };

  uulib = import ./uulib-0.9.15.nix {
    inherit cabal;
  };

  vault = import ./vault-0.2.0.1.nix {
    inherit cabal hashable unordered-containers;
  };

  vector = import ./vector-0.10.0.1.nix {
    inherit cabal deepseq primitive;
  };

  vector-algorithms = import ./vector-algorithms-0.5.4.2.nix {
    inherit cabal primitive vector;
  };

  vector-space = import ./vector-space-0.8.6.nix {
    inherit cabal Boolean MemoTrie NumInstances;
  };

  vector-space-points = import ./vector-space-points-0.1.2.0.nix {
    inherit cabal newtype vector-space;
  };

  void = import ./void-0.5.11.nix {
    inherit cabal semigroups;
  };

  vte = import ./vte-0.12.1.nix {
    inherit cabal glib gtk pango;
    inherit (nixpkgs) gtk2hsBuildtools vte;
  };

  vty = import ./vty-4.7.0.20.nix {
    inherit cabal Cabal deepseq mtl parallel parsec QuickCheck random terminfo utf8-string vector;
  };

  wai = import ./wai-1.4.0.nix {
    inherit cabal blaze-builder conduit http-types network text transformers vault;
  };

  wai-app-file-cgi = import ./wai-app-file-cgi-0.8.3.nix {
    inherit cabal attoparsec attoparsec-conduit blaze-builder blaze-html case-insensitive conduit date-cache doctest fast-logger filepath hspec http-conduit http-date http-types io-choice lifted-base mime-types network resourcet static-hash text transformers wai wai-logger warp word8;
  };

  wai-app-static = import ./wai-app-static-1.3.1.2.nix {
    inherit cabal base64-bytestring blaze-builder blaze-html blaze-markup cereal crypto-conduit cryptohash file-embed hspec http-date http-types mime-types network system-fileio system-filepath text time transformers unix-compat wai wai-test;
  };

  wai-eventsource = import ./wai-eventsource-1.3.0.4.nix {
    inherit cabal blaze-builder conduit http-types transformers wai warp;
  };

  wai-extra = import ./wai-extra-1.3.3.2.nix {
    inherit cabal ansi-terminal blaze-builder blaze-builder-conduit case-insensitive conduit data-default date-cache fast-logger hspec http-types HUnit network resourcet stringsearch text time transformers void wai wai-logger wai-test zlib zlib-bindings zlib-conduit;
  };

  wai-handler-fastcgi = import ./wai-handler-fastcgi-1.3.0.2.nix {
    inherit cabal wai wai-extra;
    inherit (nixpkgs) fcgi;
  };

  wai-handler-launch = import ./wai-handler-launch-1.3.1.4.nix {
    inherit cabal blaze-builder blaze-builder-conduit conduit http-types transformers wai warp zlib-conduit;
  };

  wai-handler-scgi = import ./wai-handler-scgi-1.3.0.2.nix {
    inherit cabal wai wai-extra;
  };

  wai-logger = import ./wai-logger-0.3.0.nix {
    inherit cabal blaze-builder byteorder case-insensitive date-cache fast-logger http-types network wai;
  };

  wai-logger-prefork = import ./wai-logger-prefork-0.3.0.nix {
    inherit cabal date-cache fast-logger http-types wai wai-logger;
  };

  wai-test = import ./wai-test-1.3.0.5.nix {
    inherit cabal blaze-builder blaze-builder-conduit case-insensitive conduit cookie http-types HUnit network text transformers wai;
  };

  warp = import ./warp-1.3.7.4.nix {
    inherit cabal blaze-builder blaze-builder-conduit case-insensitive conduit hashable hspec http-types HUnit lifted-base network network-conduit QuickCheck simple-sendfile transformers unix-compat void wai;
  };

  warp-tls = import ./warp-tls-1.4.0.1.nix {
    inherit cabal certificate conduit crypto-random-api cryptocipher network network-conduit pem tls tls-extra transformers wai warp;
  };

  webkit = import ./webkit-0.12.3.nix {
    inherit cabal cairo glib gtk pango;
    inherit (nixpkgs) gtk2hsBuildtools webkit;
  };

  web-plugins = import ./web-plugins-0.2.1.nix {
    inherit cabal mtl stm text;
  };

  web-routes = import ./web-routes-0.27.2.nix {
    inherit cabal blaze-builder http-types HUnit mtl network parsec QuickCheck split test-framework test-framework-hunit test-framework-quickcheck2 test-framework-th text utf8-string;
  };

  weighted-regexp = import ./weighted-regexp-0.3.1.2.nix {
    inherit cabal;
    inherit (nixpkgs) happy;
  };

  wl-pprint-text = import ./wl-pprint-text-1.1.0.0.nix {
    inherit cabal text;
  };

  word8 = import ./word8-0.0.2.nix {
    inherit cabal;
  };

  X11 = import ./X11-1.6.1.1.nix {
    inherit cabal data-default;
    inherit (nixpkgs) libX11 libXext libXinerama libXrandr libXrender;
  };

  X11-xft = import ./X11-xft-0.3.1.nix {
    inherit cabal utf8-string X11;
    inherit (nixpkgs) fontconfig freetype libXft pkgconfig;
  };

  xdg-basedir = import ./xdg-basedir-0.2.2.nix {
    inherit cabal filepath;
  };

  xhtml = import ./xhtml-3000.2.1.nix {
    inherit cabal;
  };

  xml = import ./xml-1.3.12.nix {
    inherit cabal text;
  };

  xml2html = import ./xml2html-0.2.0.nix {
    inherit cabal xml-conduit;
  };

  xml-conduit = import ./xml-conduit-1.1.0.3.nix {
    inherit cabal attoparsec attoparsec-conduit blaze-builder blaze-builder-conduit blaze-html blaze-markup conduit data-default failure hspec HUnit monad-control resourcet system-filepath text transformers xml-types;
  };

  xml-hamlet = import ./xml-hamlet-0.4.0.4.nix {
    inherit cabal hspec HUnit parsec shakespeare text xml-conduit;
  };

  xml-types = import ./xml-types-0.3.3.nix {
    inherit cabal text;
  };

  xmonad = import ./xmonad-0.11.nix {
    inherit cabal extensible-exceptions filepath mtl utf8-string X11;
  };

  xmonad-contrib = import ./xmonad-contrib-0.11.1.nix {
    inherit cabal extensible-exceptions mtl random utf8-string X11 X11-xft xmonad;
  };

  xss-sanitize = import ./xss-sanitize-0.3.2.nix {
    inherit cabal attoparsec css-text hspec HUnit network tagsoup text utf8-string;
  };

  yaml = import ./yaml-0.8.2.3.nix {
    inherit cabal aeson attoparsec conduit hspec HUnit resourcet text transformers unordered-containers vector;
  };

  yaml-light = import ./yaml-light-0.1.4.nix {
    inherit cabal HsSyck;
  };

  yesod = import ./yesod-1.1.9.3.nix {
    inherit cabal aeson attoparsec base64-bytestring blaze-builder blaze-html blaze-markup Cabal conduit file-embed filepath fsnotify ghc-paths hamlet http-conduit http-reverse-proxy http-types lifted-base monad-control network network-conduit optparse-applicative parsec project-template resourcet shakespeare shakespeare-css shakespeare-js shakespeare-text split system-fileio system-filepath tar text time transformers unix-compat unordered-containers wai wai-extra warp yaml yesod-auth yesod-core yesod-default yesod-form yesod-json yesod-persistent zlib;
  };

  yesod-auth = import ./yesod-auth-1.1.7.nix {
    inherit cabal aeson authenticate blaze-html blaze-markup file-embed hamlet http-conduit http-types lifted-base mime-mail network persistent persistent-template pureMD5 pwstore-fast random SHA shakespeare-css shakespeare-js text transformers unordered-containers wai yesod-core yesod-form yesod-json yesod-persistent;
  };

  yesod-auth-oauth = import ./yesod-auth-oauth-1.1.0.2.nix {
    inherit cabal authenticate-oauth lifted-base text transformers yesod-auth yesod-core yesod-form;
  };

  yesod-auth-account = import ./yesod-auth-account-1.1.0.1.nix {
    inherit cabal base64-bytestring blaze-html monad-logger mtl persistent persistent-sqlite pwstore-fast random resourcet text xml-conduit yesod yesod-auth yesod-core yesod-form yesod-persistent yesod-test;
  };

  yesod-core = import ./yesod-core-1.1.8.3.nix {
    inherit cabal aeson blaze-builder blaze-html blaze-markup case-insensitive cereal clientsession conduit cookie failure fast-logger hamlet hspec http-types HUnit lifted-base monad-control monad-logger parsec path-pieces QuickCheck random resourcet shakespeare shakespeare-css shakespeare-i18n shakespeare-js text time transformers transformers-base vector wai wai-extra wai-test yesod-routes;
  };

  yesod-default = import ./yesod-default-1.1.3.2.nix {
    inherit cabal data-default hamlet network-conduit safe shakespeare-css shakespeare-js text transformers unordered-containers wai wai-extra warp yaml yesod-core;
  };

  yesod-form = import ./yesod-form-1.2.1.3.nix {
    inherit cabal aeson attoparsec blaze-builder blaze-html blaze-markup crypto-api data-default email-validate hamlet hspec network persistent shakespeare-css shakespeare-js text time transformers wai xss-sanitize yesod-core yesod-persistent;
  };

  yesod-json = import ./yesod-json-1.1.2.2.nix {
    inherit cabal aeson attoparsec-conduit blaze-builder conduit hspec safe shakespeare-js text transformers vector wai wai-extra wai-test yesod-core yesod-routes;
  };

  yesod-markdown = import ./yesod-markdown-0.7.2.nix {
    inherit cabal blaze-html blaze-markup hamlet pandoc persistent text xss-sanitize yesod-core yesod-form;
  };

  yesod-newsfeed = import ./yesod-newsfeed-1.1.0.1.nix {
    inherit cabal blaze-html blaze-markup hamlet text time xml-conduit yesod-core;
  };

  yesod-persistent = import ./yesod-persistent-1.1.0.1.nix {
    inherit cabal persistent persistent-template transformers yesod-core;
  };

  yesod-routes = import ./yesod-routes-1.1.2.nix {
    inherit cabal hspec HUnit path-pieces text vector;
  };

  yesod-static = import ./yesod-static-1.1.2.2.nix {
    inherit cabal base64-bytestring cereal conduit crypto-conduit cryptohash file-embed hspec http-types system-filepath text transformers unix-compat wai wai-app-static yesod-core;
  };

  yesod-test = import ./yesod-test-0.3.5.nix {
    inherit cabal attoparsec blaze-builder blaze-html blaze-markup case-insensitive cookie hspec html-conduit http-types HUnit monad-control network persistent pool-conduit text time transformers wai wai-test xml-conduit xml-types;
  };

  zeromq3-haskell = import ./zeromq3-haskell-0.4.nix {
    inherit cabal ansi-terminal async checkers MonadCatchIO-transformers QuickCheck semigroups transformers;
    inherit (nixpkgs) zeromq;
  };

  zeromq-haskell = import ./zeromq-haskell-0.8.4.nix {
    inherit cabal QuickCheck test-framework test-framework-quickcheck2;
    inherit (nixpkgs) zeromq;
  };

  zip-archive = import ./zip-archive-0.1.3.4.nix {
    inherit cabal binary digest filepath HUnit mtl time utf8-string zlib;
  };

  zlib = import ./zlib-0.5.4.1.nix {
    inherit cabal;
    inherit (nixpkgs) zlib;
  };

  zlib-bindings = import ./zlib-bindings-0.1.1.1.nix {
    inherit cabal hspec HUnit QuickCheck zlib;
  };

  zlib-conduit = import ./zlib-conduit-1.0.0.nix {
    inherit cabal conduit hspec QuickCheck resourcet transformers void zlib-bindings;
  };

  zlib-enum = import ./zlib-enum-0.2.3.nix {
    inherit cabal enumerator transformers zlib-bindings;
  };

}
