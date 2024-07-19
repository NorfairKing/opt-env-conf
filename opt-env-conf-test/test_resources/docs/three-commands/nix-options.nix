{ lib }:
{
  enable = lib.mkOption {
    default = null;
    description = "enable extra";
    type = lib.types.nullOr lib.types.bool;
  };
  number = lib.mkOption {
    default = null;
    description = "number";
    type = lib.types.nullOr lib.types.int;
  };
}
