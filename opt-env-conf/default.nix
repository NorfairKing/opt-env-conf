{ mkDerivation, aeson, autodocodec, autodocodec-schema
, autodocodec-yaml, base, containers, envparse, filepath, lib, mtl
, optparse-applicative, path, path-io, safe-coloured-text
, safe-coloured-text-layout, safe-coloured-text-terminfo, selective
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
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
