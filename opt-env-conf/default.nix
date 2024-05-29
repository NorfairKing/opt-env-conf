{ mkDerivation, aeson, autodocodec, autodocodec-yaml, base
, containers, envparse, genvalidity, genvalidity-aeson
, genvalidity-containers, genvalidity-sydtest, lib, mtl
, optparse-applicative, path, path-io, QuickCheck
, safe-coloured-text, safe-coloured-text-layout, sydtest
, sydtest-discover, text, validity, validity-containers, yaml
}:
mkDerivation {
  pname = "opt-env-conf";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-yaml base containers envparse mtl
    optparse-applicative path path-io safe-coloured-text
    safe-coloured-text-layout text validity validity-containers yaml
  ];
  testHaskellDepends = [
    aeson base containers envparse genvalidity genvalidity-aeson
    genvalidity-containers genvalidity-sydtest optparse-applicative
    QuickCheck safe-coloured-text sydtest text yaml
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
