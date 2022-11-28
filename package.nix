{ mkDerivation, ansi-wl-pprint, base, bytestring, classy-prelude
, composite-base, composite-opaleye, dlist, exceptions, fast-logger
, hpack, hspec, lens, lib, monad-control, monad-logger, old-locale
, opaleye, optparse-applicative, postgresql-simple, process
, product-profunctors, template-haskell, text, these, these-lens
, thyme, transformers-base, vector-space
}:
mkDerivation {
  pname = "refurb";
  version = "0.3.0.1";
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
  homepage = "https://github.com/ConferOpenSource/refurb#readme";
  description = "Tools for maintaining a database";
  license = lib.licenses.bsd3;
}
