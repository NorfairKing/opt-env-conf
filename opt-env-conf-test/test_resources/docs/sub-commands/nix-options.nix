{ lib }:
{
  name = lib.mkOption {
    default = null;
    description = "name";
    type = lib.types.nullOr lib.types.str;
  };
}
