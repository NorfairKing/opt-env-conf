{ lib }:
{
  int = lib.mkOption {
    default = null;
    description = "int or string";
    type = lib.types.nullOr lib.types.int;
  };
  other = lib.mkOption {
    default = null;
    description = "int or string";
    type = lib.types.nullOr lib.types.bool;
  };
  string = lib.mkOption {
    default = null;
    description = "int or string";
    type = lib.types.nullOr lib.types.str;
  };
}
