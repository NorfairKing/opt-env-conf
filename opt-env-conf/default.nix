{ mkDerivation, aeson, autodocodec, autodocodec-schema
, autodocodec-yaml, base, containers, envparse, filepath
, genvalidity, genvalidity-aeson, genvalidity-containers
, genvalidity-sydtest, genvalidity-text, lib, mtl
, optparse-applicative, path, path-io, pretty-show, QuickCheck
, safe-coloured-text, safe-coloured-text-layout
, safe-coloured-text-terminfo, selective, sydtest, sydtest-discover
, text, validity, validity-containers, yaml
}:
mkDerivation {
  pname = "opt-env-conf";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-schema autodocodec-yaml base
    containers envparse filepath mtl optparse-applicative path path-io
    safe-coloured-text safe-coloured-text-layout
    safe-coloured-text-terminfo selective text validity
    validity-containers yaml
  ];
  testHaskellDepends = [
    aeson base containers envparse genvalidity genvalidity-aeson
    genvalidity-containers genvalidity-sydtest genvalidity-text
    optparse-applicative pretty-show QuickCheck safe-coloured-text
    sydtest text yaml
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
