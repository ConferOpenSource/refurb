{ mkDerivation, ansi-wl-pprint, base, bytestring, classy-prelude
, composite-base, composite-opaleye, dlist, exceptions, fast-logger
, hspec, lens, monad-control, monad-logger, old-locale, opaleye
, optparse-applicative, postgresql-simple, process
, product-profunctors, stdenv, template-haskell, text, these, thyme
, transformers-base, vector-space
}:
mkDerivation {
  pname = "refurb";
  version = "0.2.2.0";
  src = ./.;
  libraryHaskellDepends = [
    ansi-wl-pprint base bytestring classy-prelude composite-base
    composite-opaleye dlist exceptions fast-logger lens monad-control
    monad-logger old-locale opaleye optparse-applicative
    postgresql-simple process product-profunctors template-haskell text
    these thyme transformers-base vector-space
  ];
  testHaskellDepends = [
    ansi-wl-pprint base bytestring classy-prelude composite-base
    composite-opaleye dlist exceptions fast-logger hspec lens
    monad-control monad-logger old-locale opaleye optparse-applicative
    postgresql-simple process product-profunctors template-haskell text
    these thyme transformers-base vector-space
  ];
  homepage = "https://github.com/ConferHealth/refurb#readme";
  description = "Tools for maintaining a database";
  license = stdenv.lib.licenses.bsd3;
}
