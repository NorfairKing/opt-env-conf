{ mkDerivation, aeson, autodocodec, autodocodec-nix
, autodocodec-schema, autodocodec-yaml, base, containers, hashable
, lib, mtl, path, path-io, safe-coloured-text
, safe-coloured-text-layout, safe-coloured-text-terminfo, selective
, text, validity, validity-containers
}:
mkDerivation {
  pname = "opt-env-conf";
  version = "0.6.0.3";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-nix autodocodec-schema
    autodocodec-yaml base containers hashable mtl path path-io
    safe-coloured-text safe-coloured-text-layout
    safe-coloured-text-terminfo selective text validity
    validity-containers
  ];
  homepage = "https://github.com/NorfairKing/opt-env-conf#readme";
  description = "Settings parsing for Haskell: command-line arguments, environment variables, and configuration values";
  license = lib.licenses.lgpl3Only;
}
