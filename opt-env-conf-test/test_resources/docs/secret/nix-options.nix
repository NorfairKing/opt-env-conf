{ lib }:
{
  first-secret-file = lib.mkOption {
    default = null;
    description = "First example secret";
    type = lib.types.nullOr lib.types.str;
  };
  second-secret = lib.mkOption {
    default = null;
    description = "Second example secret, bare or in a file, only conf";
    type = lib.types.nullOr lib.types.str;
  };
  second-secret-file = lib.mkOption {
    default = null;
    description = "Second example secret, bare or in a file, only conf";
    type = lib.types.nullOr lib.types.str;
  };
}
