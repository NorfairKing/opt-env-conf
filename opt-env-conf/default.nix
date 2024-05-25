{ mkDerivation, aeson, autodocodec, autodocodec-yaml, base
, containers, envparse, genvalidity-sydtest, lib
, optparse-applicative, path, path-io, QuickCheck, sydtest
, sydtest-discover, text, validity, validity-containers, yaml
}:
mkDerivation {
  pname = "opt-env-conf";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-yaml base containers envparse
    optparse-applicative path path-io text validity validity-containers
    yaml
  ];
  testHaskellDepends = [
    base containers envparse genvalidity-sydtest optparse-applicative
    QuickCheck sydtest yaml
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
