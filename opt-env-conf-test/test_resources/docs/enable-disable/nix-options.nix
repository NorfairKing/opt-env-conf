{ lib }:
{
  example = lib.mkOption {
    default = null;
    description = "Example of an enable/disable switch";
    type = lib.types.nullOr lib.types.bool;
  };
}
