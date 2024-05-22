{ mkDerivation, aeson, autodocodec, autodocodec-yaml, base
, envparse, lib, optparse-applicative, path, path-io, sydtest
, sydtest-discover, text, yaml
}:
mkDerivation {
  pname = "opt-env-conf";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-yaml base envparse
    optparse-applicative path path-io text yaml
  ];
  testHaskellDepends = [
    base envparse optparse-applicative sydtest yaml
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
