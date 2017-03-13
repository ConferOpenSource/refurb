{ mkDerivation, ansi-wl-pprint, base, bytestring, classy-prelude
, composite-base, composite-opaleye, dlist, fast-logger, Frames
, lens, monad-logger, old-locale, opaleye, optparse-applicative
, postgresql-simple, process, product-profunctors, stdenv
, template-haskell, these, thyme, vector-space
}:
mkDerivation {
  pname = "refurb";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    ansi-wl-pprint base bytestring classy-prelude composite-base
    composite-opaleye dlist fast-logger Frames lens monad-logger
    old-locale opaleye optparse-applicative postgresql-simple process
    product-profunctors template-haskell these thyme vector-space
  ];
  homepage = "https://github.com/ConferHealth/refurb#readme";
  description = "Tools for maintaining a database";
  license = stdenv.lib.licenses.unfree;
}
