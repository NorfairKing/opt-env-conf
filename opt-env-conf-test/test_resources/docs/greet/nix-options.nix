{ lib }:
{
  greeting = lib.mkOption {
    default = null;
    description = "Greeting to use";
    type = lib.types.nullOr lib.types.str;
  };
  polite = lib.mkOption {
    default = null;
    description = "Whether to be polite";
    type = lib.types.nullOr lib.types.bool;
  };
}
