{ mkDerivation, ansi-wl-pprint, base, bytestring, classy-prelude
, composite-base, composite-opaleye, dlist, fast-logger, hspec
, lens, monad-logger, old-locale, opaleye, optparse-applicative
, postgresql-simple, process, product-profunctors, stdenv
, template-haskell, text, these, thyme, vector-space
}:
mkDerivation {
  pname = "refurb";
  version = "0.2.2.0";
  src = ./.;
  libraryHaskellDepends = [
    ansi-wl-pprint base bytestring classy-prelude composite-base
    composite-opaleye dlist fast-logger lens monad-logger old-locale
    opaleye optparse-applicative postgresql-simple process
    product-profunctors template-haskell text these thyme vector-space
  ];
  testHaskellDepends = [
    ansi-wl-pprint base bytestring classy-prelude composite-base
    composite-opaleye dlist fast-logger hspec lens monad-logger
    old-locale opaleye optparse-applicative postgresql-simple process
    product-profunctors template-haskell text these thyme vector-space
  ];
  homepage = "https://github.com/ConferHealth/refurb#readme";
  description = "Tools for maintaining a database";
  license = stdenv.lib.licenses.bsd3;
}
