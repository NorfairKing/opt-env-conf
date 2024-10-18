{ lib }:
{
  example-secret-file = lib.mkOption {
    default = null;
    description = "example text file";
    type = lib.types.nullOr lib.types.str;
  };
}
