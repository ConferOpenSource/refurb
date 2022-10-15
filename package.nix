{ mkDerivation, ansi-wl-pprint, base, bytestring, classy-prelude
, composite-base, composite-opaleye, dlist, exceptions, fast-logger
, hpack, hspec, lens, monad-control, monad-logger, old-locale
, opaleye, optparse-applicative, postgresql-simple, process
, product-profunctors, stdenv, template-haskell, text, these
, these-lens, thyme, transformers-base, vector-space
}:
mkDerivation {
  pname = "refurb";
  version = "0.2.3.0";
  src = ./.;
  libraryHaskellDepends = [
    ansi-wl-pprint base bytestring classy-prelude composite-base
    composite-opaleye dlist exceptions fast-logger lens monad-control
    monad-logger old-locale opaleye optparse-applicative
    postgresql-simple process product-profunctors template-haskell text
    these these-lens thyme transformers-base vector-space
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    ansi-wl-pprint base bytestring classy-prelude composite-base
    composite-opaleye dlist exceptions fast-logger hspec lens
    monad-control monad-logger old-locale opaleye optparse-applicative
    postgresql-simple process product-profunctors template-haskell text
    these these-lens thyme transformers-base vector-space
  ];
  prePatch = "hpack";
  homepage = "https://github.com/ConferHealth/refurb#readme";
  description = "Tools for maintaining a database";
  license = stdenv.lib.licenses.bsd3;
}
