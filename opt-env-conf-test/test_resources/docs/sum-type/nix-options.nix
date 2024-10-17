{ lib }:
{
  sum-type = lib.mkOption {
    default = null;
    description = "example";
    type = lib.types.nullOr lib.types.str;
  };
}
