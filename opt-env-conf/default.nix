{ mkDerivation, aeson, autodocodec, autodocodec-schema
, autodocodec-yaml, base, containers, filepath, lib, mtl, path-io
, safe-coloured-text, safe-coloured-text-layout
, safe-coloured-text-terminfo, selective, text, validity
, validity-containers
}:
mkDerivation {
  pname = "opt-env-conf";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-schema autodocodec-yaml base
    containers filepath mtl path-io safe-coloured-text
    safe-coloured-text-layout safe-coloured-text-terminfo selective
    text validity validity-containers
  ];
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
