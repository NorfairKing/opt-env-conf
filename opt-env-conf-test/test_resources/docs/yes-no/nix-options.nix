{ lib }:
{
  example = lib.mkOption {
    default = null;
    description = "Example of a yes/no switch";
    type = lib.types.nullOr lib.types.bool;
  };
}
